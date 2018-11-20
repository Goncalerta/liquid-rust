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
use super::LiquidOptions;
use super::ParseBlock;
use super::ParseTag;

use pest::Parser;

mod pest {
    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    pub struct LiquidParser;
}

use self::pest::*;

type Pair<'a> = ::pest::iterators::Pair<'a, Rule>;
type Pairs<'a> = ::pest::iterators::Pairs<'a, Rule>;

fn convert_pest_error(err: ::pest::error::Error<Rule>) -> Error {
    let err = err.renamed_rules(|&rule| match rule {
        Rule::LesserThan => "\"<\"".to_string(),
        Rule::GreaterThan => "\">\"".to_string(),
        Rule::LesserThanEquals => "\"<=\"".to_string(),
        Rule::GreaterThanEquals => "\">=\"".to_string(),
        Rule::Equals => "\"==\"".to_string(),
        Rule::NotEquals => "\"!=\"".to_string(),
        Rule::LesserThanGreaterThan => "\"<>\"".to_string(),
        Rule::Assign => "\"=\"".to_string(),
        Rule::Comma => "\",\"".to_string(),
        Rule::Colon => "\":\"".to_string(),
        other => format!("{:?}", other),
    });
    Error::with_msg(err.to_string())
}

fn error_from_pair(pair: Pair, msg: String) -> Error {
    let pest_error = ::pest::error::Error::new_from_span(
        ::pest::error::ErrorVariant::CustomError { message: msg },
        pair.as_span(),
    );
    convert_pest_error(pest_error)
}

pub fn parse(text: &str, options: &LiquidOptions) -> Result<Vec<Box<Renderable>>> {
    let mut liquid = LiquidParser::parse(Rule::LiquidFile, text)
        .map_err(convert_pest_error)?
        .next()
        .expect("Unwrapping LiquidFile to access the elements.")
        .into_inner();

    let mut renderables = Vec::new();

    while let Some(element) = liquid.next() {
        if element.as_rule() == Rule::EOI {
            break;
        }

        renderables.push(BlockElement::parse_pair(
            element.into(),
            &mut liquid,
            options,
        )?);
    }
    Ok(renderables)
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

        let element = self.iter.next().expect("File shouldn't end before EOI.");

        if element.as_rule() == Rule::EOI {
            return Err(error_from_pair(
                element,
                format!("Unclosed block. {{% {} %}} tag expected.", self.end_name),
            ));
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
    name: Pair<'a>,
    tokens: TagTokenIter<'a>,
    as_str: &'a str,
}
impl<'a> From<Pair<'a>> for Tag<'a> {
    fn from(element: Pair<'a>) -> Self {
        if element.as_rule() != Rule::Tag {
            panic!("Only rule Tag can be converted to Tag.");
        }
        let as_str = element.as_str();
        let mut tag = element.into_inner();
        let name = tag.next().expect("A tag starts with an identifier.");
        let tokens = TagTokenIter::new(&name, tag);

        Tag {
            name,
            tokens,
            as_str,
        }
    }
}
impl<'a> Tag<'a> {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
    pub fn tokens(&mut self) -> &mut TagTokenIter<'a> {
        &mut self.tokens
    }
    pub fn into_tokens(self) -> TagTokenIter<'a> {
        self.tokens
    }
    pub fn as_str(&self) -> &str {
        self.as_str
    }
    pub fn parse(
        self,
        tag_block: &mut TagBlock,
        options: &LiquidOptions,
    ) -> Result<Box<Renderable>> {
        self.parse_pair(&mut tag_block.iter, options)
    }

    fn parse_pair(
        self,
        next_elements: &mut Iterator<Item = Pair>,
        options: &LiquidOptions,
    ) -> Result<Box<Renderable>> {
        let (name, tokens) = (self.name, self.tokens);
        let position = name.as_span();
        let name = name.as_str();

        if options.tags.contains_key(name) {
            options.tags[name].parse(name, tokens, options)
        } else if options.blocks.contains_key(name) {
            let mut block = TagBlock::new(name, next_elements);
            let renderables = options.blocks[name].parse(name, tokens, &mut block, options)?;
            block.close()?;
            Ok(renderables)
        } else {
            let pest_error = ::pest::error::Error::new_from_span(
                ::pest::error::ErrorVariant::CustomError {
                    message: "Unknown tag.".to_string(),
                },
                position,
            );
            Err(convert_pest_error(pest_error))
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

    fn parse_pair(
        self,
        next_elements: &mut Iterator<Item = Pair>,
        options: &LiquidOptions,
    ) -> Result<Box<Renderable>> {
        match self {
            BlockElement::Raw(raw) => Ok(raw.to_renderable()),
            BlockElement::Tag(tag) => tag.parse_pair(next_elements, options),
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
pub struct TagTokenIter<'a> {
    iter: Box<Iterator<Item = TagToken<'a>> + 'a>,
    position: ::pest::Position<'a>,
}
impl<'a> Iterator for TagTokenIter<'a> {
    type Item = TagToken<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|next| {
            self.position = next.token.as_span().end_pos();
            next
        })
    }
}
impl<'a> TagTokenIter<'a> {
    fn new(name: &Pair<'a>, tokens: Pairs<'a>) -> Self {
        TagTokenIter {
            iter: Box::new(tokens.map(TagToken::from)),
            position: name.as_span().end_pos(),
        }
    }

    pub fn expect_next(&mut self, error_msg: &str) -> Result<TagToken<'a>> {
        self.next().ok_or_else(|| {
            let pest_error = ::pest::error::Error::new_from_pos(
                ::pest::error::ErrorVariant::CustomError {
                    message: error_msg.to_string(),
                },
                self.position.clone(),
            );
            convert_pest_error(pest_error)
        })
    }
}

/// An interface to parse tokens inside Tags without exposing the Pair structures
pub struct TagToken<'a> {
    token: Pair<'a>,
    expected: Vec<Rule>,
}

impl<'a> From<Pair<'a>> for TagToken<'a> {
    fn from(token: Pair<'a>) -> Self {
        TagToken {
            token,
            expected: Vec::new(),
        }
    }
}

impl<'a> TagToken<'a> {
    pub fn raise_error(self) -> Error {
        let pest_error = ::pest::error::Error::new_from_span(
            ::pest::error::ErrorVariant::ParsingError {
                positives: self.expected,
                negatives: vec![self.token.as_rule()],
            },
            self.token.as_span(),
        );
        convert_pest_error(pest_error)
    }

    pub fn raise_custom_error(self, msg: &str) -> Error {
        let pest_error = ::pest::error::Error::new_from_span(
            ::pest::error::ErrorVariant::CustomError {
                message: msg.to_string(),
            },
            self.token.as_span(),
        );
        convert_pest_error(pest_error)
    }

    fn unwrap_filter_chain(&mut self) -> std::result::Result<Pair<'a>, ()> {
        let token = self.token.clone();

        if token.as_rule() != Rule::FilterChain {
            return Err(());
        }

        Ok(token)
    }

    fn unwrap_value(&mut self) -> std::result::Result<Pair<'a>, ()> {
        let filterchain = self.unwrap_filter_chain()?;

        let mut chain = filterchain.into_inner();
        let value = chain.next().expect("Unwrapping value out of Filterchain.");
        if chain.next().is_some() {
            // There are filters: it can't be a value
            return Err(());
        }

        Ok(value)
    }

    fn unwrap_variable(&mut self) -> std::result::Result<Pair<'a>, ()> {
        let value = self.unwrap_value()?;

        let variable = value
            .into_inner()
            .next()
            .expect("A value is made of one token.");

        if variable.as_rule() != Rule::Variable {
            return Err(());
        }

        Ok(variable)
    }

    fn unwrap_identifier(&mut self) -> std::result::Result<Pair<'a>, ()> {
        let variable = self.unwrap_variable()?;

        let mut indexes = variable.into_inner();
        let identifier = indexes
            .next()
            .expect("Unwrapping identifier out of variable.");
        if indexes.next().is_some() {
            // There are indexes: it can't be a value
            return Err(());
        }

        Ok(identifier)
    }

    fn unwrap_literal(&mut self) -> std::result::Result<Pair<'a>, ()> {
        let value = self.unwrap_value()?;

        let literal = value
            .into_inner()
            .next()
            .expect("A value is made of one token.");

        if literal.as_rule() != Rule::Literal {
            return Err(());
        }

        Ok(literal)
    }

    pub fn expect_filter_chain(mut self) -> std::result::Result<FilterChain, Self> {
        let filterchain = self.unwrap_filter_chain().map_err(|_| {
            self.expected.push(Rule::FilterChain);
            self
        })?;

        Ok(parse_filter_chain(filterchain))
    }

    pub fn expect_value(mut self) -> std::result::Result<Expression, Self> {
        let value = self.unwrap_value().map_err(|_| {
            self.expected.push(Rule::Value);
            self
        })?;

        Ok(parse_value(value))
    }

    pub fn expect_variable(mut self) -> std::result::Result<Variable, Self> {
        let variable = self.unwrap_variable().map_err(|_| {
            self.expected.push(Rule::Variable);
            self
        })?;

        Ok(parse_variable(variable))
    }

    pub fn expect_identifier(mut self) -> std::result::Result<&'a str, Self> {
        let identifier = self.unwrap_identifier().map_err(|_| {
            self.expected.push(Rule::Identifier);
            self
        })?;

        Ok(identifier.as_str())
    }

    pub fn expect_literal(mut self) -> std::result::Result<Value, Self> {
        let literal = self.unwrap_literal().map_err(|_| {
            self.expected.push(Rule::Literal);
            self
        })?;

        Ok(Value::scalar(parse_literal(literal)))
    }

    pub fn expect_range(mut self) -> std::result::Result<(Expression, Expression), Self> {
        let token = self.token.clone();

        if token.as_rule() != Rule::Range {
            self.expected.push(Rule::Range);
            return Err(self);
        }

        let mut range = token.into_inner();
        Ok((
            parse_value(range.next().expect("start")),
            parse_value(range.next().expect("end")),
        ))
    }

    pub fn expect_str(self, expected: &str) -> std::result::Result<(), Self> {
        if self.as_str() == expected {
            Ok(())
        } else {
            // TODO change self to be aware that `expected` was expected.
            Err(self)
        }
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
