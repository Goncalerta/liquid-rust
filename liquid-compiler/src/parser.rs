//! Parser
//!
//! This module contains functions than can be used for writing plugins
//! but should be ignored for simple usage.

use liquid_interpreter::Expression;
use liquid_interpreter::Renderable;
use liquid_interpreter::Text;
use liquid_interpreter::Variable;
use liquid_interpreter::{FilterCall, FilterChain};
use liquid_value::Scalar;
use liquid_value::Value;

use super::error::{Error, Result};
use super::BoxedTagParser;
use super::LiquidOptions;
use super::ParseBlock;
use super::ParseTag;

use std::collections::HashMap;

use pest::Parser;

mod pest {
    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    pub struct LiquidParser;
}

use self::pest::*;

type Pair<'a> = ::pest::iterators::Pair<'a, Rule>;

pub fn parse(text: &str, options: &LiquidOptions) -> Result<Vec<Box<Renderable>>> {
    let mut liquid = LiquidParser::parse(Rule::LiquidFile, text)
        .unwrap() // TODO do NOT unwrap error
        .next()
        .expect("Unwrapping LiquidFile to access the elements.")
        .into_inner();

    let mut renderables = Vec::new();

    while let Some(element) = liquid.next() {
        if element.as_rule() == Rule::EOI {
            break;
        }

        renderables.push(parse_element(element, &mut liquid, options)?);
    }
    Ok(renderables)
}

fn parse_element<'a>(
    element: Pair<'a>,
    next_elements: &mut Iterator<Item = Pair<'a>>,
    options: &LiquidOptions,
) -> Result<Box<Renderable>> {
    match element.as_rule() {
        Rule::Expression => {
            let filter_chain = element
                .into_inner()
                .next()
                .expect("An expression consists of one filterchain.");

            Ok(Box::new(parse_filter_chain(filter_chain)))
        }
        Rule::Tag => Ok(parse_tag(element, next_elements, options)?),
        Rule::Raw => Ok(Box::new(Text::new(element.as_str()))),
        _ => panic!("Expected Expression | Tag | Raw."),
    }
}

fn parse_literal(literal: Pair) -> Scalar {
    if literal.as_rule() != Rule::Literal {
        panic!("Expected literal.");
    }

    let literal = literal
        .into_inner()
        .next()
        .expect("Get into the rule inside literal.");

    match literal.as_rule() {
        Rule::StringLiteral => {
            let literal = literal.as_str();
            let trim_quotes = &literal[1..literal.len() - 1];

            Scalar::new(trim_quotes.to_owned())
        }
        Rule::IntegerLiteral => Scalar::new(
            literal
                .as_str()
                .parse::<i32>()
                .expect("Matches are parseable as integers."),
        ),
        Rule::FloatLiteral => Scalar::new(
            literal
                .as_str()
                .parse::<f64>()
                .expect("Matches are parseable as floats."),
        ),
        Rule::BooleanLiteral => Scalar::new(
            literal
                .as_str()
                .parse::<bool>()
                .expect("Matches are parseable as bools."),
        ),
        _ => unreachable!(),
    }
}

fn parse_variable(variable: Pair) -> Variable {
    if variable.as_rule() != Rule::Variable {
        panic!("Expected variable.");
    }

    let mut indexes = variable.into_inner();

    let first_identifier = indexes
        .next()
        .expect("A variable starts with an identifier.")
        .as_str()
        .to_owned();
    let mut variable = Variable::with_literal(first_identifier);

    let indexes = indexes.map(|index| match index.as_rule() {
        Rule::Identifier => Expression::with_literal(index.as_str().to_owned()),
        Rule::Value => parse_value(index),
        _ => unreachable!(),
    });

    variable.extend(indexes);
    variable
}

fn parse_value(value: Pair) -> Expression {
    if value.as_rule() != Rule::Value {
        panic!("Expected value.");
    }

    let value = value.into_inner().next().expect("Get inside the value.");

    match value.as_rule() {
        Rule::Literal => Expression::with_literal(parse_literal(value)),
        Rule::Variable => Expression::Variable(parse_variable(value)),
        _ => unreachable!(),
    }
}

fn parse_filter(filter: Pair) -> FilterCall {
    if filter.as_rule() != Rule::Filter {
        panic!("Expected a filter.");
    }

    let mut filter = filter.into_inner();
    let name = filter.next().expect("A filter always has a name.").as_str();
    let args = filter.map(parse_value).collect();

    FilterCall::new(name, args)
}

fn parse_filter_chain(chain: Pair) -> FilterChain {
    if chain.as_rule() != Rule::FilterChain {
        panic!("Expected an expression with filters.");
    }

    let mut chain = chain.into_inner();
    let entry = parse_value(
        chain
            .next()
            .expect("A filterchain always has starts by a value."),
    );
    let filters = chain.map(parse_filter).collect();

    FilterChain::new(entry, filters)
}

fn parse_tag<'a>(
    tag: Pair<'a>,
    next_elements: &mut Iterator<Item = Pair<'a>>,
    options: &LiquidOptions,
) -> Result<Box<Renderable>> {
    if tag.as_rule() != Rule::Tag {
        panic!("Expected a tag.");
    }

    let mut tag = tag.into_inner();
    let name = tag
        .next()
        .expect("A tag starts with an identifier.")
        .as_str();
    let mut tokens = tag.map(TagToken::from);

    if options.tags.contains_key(name) {
        options.tags[name].parse(name, &mut tokens, options)
    } else if options.blocks.contains_key(name) {
        let mut block = TagBlock::new(name, next_elements);
        let renderables = options.blocks[name].parse(name, &mut tokens, &mut block, options);
        block.close()?;
        renderables
    } else {
        panic!("Errors not implemented. Unknown tag.")
    }
}

/// An interface to parse elements inside blocks without exposing the Pair structures
pub struct TagBlock<'a: 'b, 'b> {
    name: &'b str,
    end_name: String,
    iter: &'b mut Iterator<Item = Pair<'a>>,
    nesting_depth: u32,
}

impl<'a, 'b> TagBlock<'a, 'b> {
    fn new(name: &'b str, next_elements: &'b mut Iterator<Item = Pair<'a>>) -> Self {
        let end_name = format!("end{}", name);
        TagBlock {
            name,
            end_name,
            iter: next_elements,
            nesting_depth: 1,
        }
    }

    pub fn next(&mut self) -> Result<Option<BlockElement<'a>>> {
        if self.nesting_depth == 0 {
            return Ok(None);
        }

        let element = self
            .iter
            .next()
            .expect("The file must not end before an EOI.");
        if element.as_rule() == Rule::EOI {
            return panic!("Errors not implemented. Unclosed block.");
        }
        if element.as_rule() == Rule::Tag {
            let nested_tag_name = element
                .clone() // Maybe there is a better way?
                .into_inner()
                .next()
                .expect("Tags start by their identifier.")
                .as_str();
            if self.name == nested_tag_name {
                self.nesting_depth += 1;
            } else if self.end_name == nested_tag_name {
                self.nesting_depth -= 1;
            }
            if self.nesting_depth == 0 {
                return Ok(None);
            }
        }
        Ok(Some(element.into()))
    }

    fn close(mut self) -> Result<()> {
        while let Some(_) = self.next()? {}
        Ok(())
    }

    pub fn parse(&mut self, options: &LiquidOptions) -> Result<Vec<Box<Renderable>>> {
        let mut renderables = Vec::new();
        while let Some(r) = self.parse_next(options)? {
            renderables.push(r);
        }
        Ok(renderables)
    }

    pub fn parse_next(&mut self, options: &LiquidOptions) -> Result<Option<Box<Renderable>>> {
        match self.next()? {
            None => Ok(None),
            Some(element) => Ok(Some(element.parse(self, options)?)),
        }
    }
}

pub struct Raw<'a> {
    text: &'a str,
}
impl<'a> From<Pair<'a>> for Raw<'a> {
    fn from(element: Pair<'a>) -> Self {
        if element.as_rule() != Rule::Raw {
            panic!("Only rule Raw can be converted to Raw.");
        }
        Raw {
            text: element.as_str(),
        }
    }
}
impl<'a> Into<&'a str> for Raw<'a> {
    fn into(self) -> &'a str {
        self.to_str()
    }
}
impl<'a> Raw<'a> {
    pub fn to_renderable(self) -> Box<Renderable> {
        Box::new(Text::new(self.to_str()))
    }

    pub fn to_str(self) -> &'a str {
        self.as_str()
    }

    pub fn as_str(&self) -> &'a str {
        self.text
    }
}

pub struct Tag<'a> {
    name: &'a str,
    tokens: Box<Iterator<Item = TagToken<'a>> + 'a>,
    as_str: &'a str,
}
impl<'a> From<Pair<'a>> for Tag<'a> {
    fn from(element: Pair<'a>) -> Self {
        if element.as_rule() != Rule::Tag {
            panic!("Only rule Tag can be converted to Tag.");
        }
        let as_str = element.as_str();
        let mut tag = element.into_inner();
        let name = tag
            .next()
            .expect("A tag starts with an identifier.")
            .as_str();
        let tokens = Box::new(tag.map(TagToken::from));

        Tag {
            name,
            tokens,
            as_str,
        }
    }
}
impl<'a> Tag<'a> {
    pub fn name(&self) -> &str {
        self.name
    }
    pub fn tokens(&mut self) -> &mut Iterator<Item = TagToken<'a>> {
        &mut self.tokens
    }
    pub fn as_str(&self) -> &str {
        self.as_str
    }
    pub fn parse(
        mut self,
        next_elements: &mut TagBlock,
        options: &LiquidOptions,
    ) -> Result<Box<Renderable>> {
        let (name, tokens) = (self.name, &mut self.tokens);

        let next_elements = &mut next_elements.iter;

        if options.tags.contains_key(name) {
            options.tags[name].parse(name, tokens, options)
        } else if options.blocks.contains_key(name) {
            let mut block = TagBlock::new(name, next_elements);
            let renderables = options.blocks[name].parse(name, tokens, &mut block, options);
            block.close()?;
            renderables
        } else {
            panic!("Errors not implemented. Unknown tag.")
        }
    }
}

pub struct Exp<'a> {
    element: Pair<'a>,
}
impl<'a> From<Pair<'a>> for Exp<'a> {
    fn from(element: Pair<'a>) -> Self {
        if element.as_rule() != Rule::Expression {
            panic!("Only rule Expression can be converted to Expression.");
        }
        Exp { element }
    }
}
impl<'a> Exp<'a> {
    pub fn parse(self) -> Result<Box<Renderable>> {
        let filter_chain = self
            .element
            .into_inner()
            .next()
            .expect("An expression consists of one filterchain.");

        Ok(Box::new(parse_filter_chain(filter_chain)))
    }
    pub fn as_str(&self) -> &str {
        self.element.as_str()
    }
}

pub enum BlockElement<'a> {
    Raw(Raw<'a>),
    Tag(Tag<'a>),
    Expression(Exp<'a>),
}
impl<'a> From<Pair<'a>> for BlockElement<'a> {
    fn from(element: Pair<'a>) -> Self {
        match element.as_rule() {
            Rule::Raw => BlockElement::Raw(element.into()),
            Rule::Tag => BlockElement::Tag(element.into()),
            Rule::Expression => BlockElement::Expression(element.into()),
            _ => panic!("Only rules Raw | Tag | Expression can be converted to BlockElement."),
        }
    }
}

impl<'a> BlockElement<'a> {
    pub fn parse(
        self,
        block: &mut TagBlock<'a, '_>,
        options: &LiquidOptions,
    ) -> Result<Box<Renderable>> {
        match self {
            BlockElement::Raw(raw) => Ok(raw.to_renderable()),
            BlockElement::Tag(tag) => tag.parse(block, options),
            BlockElement::Expression(exp) => exp.parse(),
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            BlockElement::Raw(raw) => raw.as_str(),
            BlockElement::Tag(tag) => tag.as_str(),
            BlockElement::Expression(exp) => exp.as_str(),
        }
    }
}

/// An interface to parse tokens inside Tags without exposing the Pair structures
pub struct TagToken<'a> {
    token: Pair<'a>,
}

impl<'a> From<Pair<'a>> for TagToken<'a> {
    fn from(token: Pair<'a>) -> Self {
        TagToken { token }
    }
}

impl<'a> TagToken<'a> {
    fn unwrap_filter_chain(&self) -> Result<Pair<'a>> {
        let token = self.token.clone();

        if token.as_rule() != Rule::FilterChain {
            return panic!("Error handling is not implemented. Expected Filterchain.");
        }

        Ok(token)
    }

    fn unwrap_value(&self) -> Result<Pair<'a>> {
        let filterchain = self.unwrap_filter_chain()?;

        let mut chain = filterchain.into_inner();
        let value = chain.next().expect("Unwrapping value out of Filterchain.");
        if chain.next().is_some() {
            // There are filters: it can't be a value
            return panic!("Error handling is not implemented. Expected value.");
        }

        Ok(value)
    }

    fn unwrap_variable(&self) -> Result<Pair<'a>> {
        let value = self.unwrap_value()?;

        let variable = value
            .into_inner()
            .next()
            .expect("A value is made of one token.");

        if variable.as_rule() != Rule::Variable {
            return panic!("Error handling is not implemented. Expected variable.");
        }

        Ok(variable)
    }

    fn unwrap_identifier(&self) -> Result<Pair<'a>> {
        let variable = self.unwrap_variable()?;

        let mut indexes = variable.into_inner();
        let identifier = indexes
            .next()
            .expect("Unwrapping identifier out of variable.");
        if indexes.next().is_some() {
            // There are indexes: it can't be a value
            return panic!("Error handling is not implemented. Expected identifier.");
        }

        Ok(identifier)
    }

    fn unwrap_literal(&self) -> Result<Pair<'a>> {
        let value = self.unwrap_value()?;

        let literal = value
            .into_inner()
            .next()
            .expect("A value is made of one token.");

        if literal.as_rule() != Rule::Literal {
            return panic!("Error handling is not implemented. Expected literal.");
        }

        Ok(literal)
    }

    pub fn expect_filter_chain(&self) -> Result<FilterChain> {
        let filterchain = self.unwrap_filter_chain()?;

        Ok(parse_filter_chain(filterchain))
    }

    pub fn expect_value(&self) -> Result<Expression> {
        let value = self.unwrap_value()?;

        Ok(parse_value(value))
    }

    pub fn expect_variable(&self) -> Result<Variable> {
        let variable = self.unwrap_variable()?;

        Ok(parse_variable(variable))
    }

    pub fn expect_identifier(&self) -> Result<&'a str> {
        let identifier = self.unwrap_identifier()?;

        Ok(identifier.as_str())
    }

    pub fn expect_literal(&self) -> Result<Value> {
        let literal = self.unwrap_literal()?;

        Ok(Value::scalar(parse_literal(literal)))
    }

    pub fn expect_range(&self) -> Result<(Expression, Expression)> {
        let token = self.token.clone();

        if token.as_rule() != Rule::Range {
            return panic!("Error handling is not implemented. Expected Filterchain.");
        }

        let mut range = token.into_inner();
        Ok((
            parse_value(range.next().expect("start")),
            parse_value(range.next().expect("end")),
        ))
    }

    pub fn as_str(&self) -> &str {
        self.token.as_str().trim()
    }
}

#[cfg(test)]
mod test_parse_output {
    use super::*;

    // TODO tests
}
