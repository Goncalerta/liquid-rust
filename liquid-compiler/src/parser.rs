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
            let mut filter_chain = element
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
    let tokens = TagTokens::new(&mut tag);

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


/// An interface to parse elements inside blocks without exposing the Pair structures
// TODO find if there is a better way to store the blocks instead of a vector of pairs.
pub struct TagBlock<'a:'b, 'b> {
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

    fn next(&mut self) -> Result<Option<Pair<'a>>> {
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
        Ok(Some(element))
    }

    fn close(mut self) -> Result<()> {
        while let Some(_) = self.next()? {}
        Ok(())
    }

    pub fn parse(&mut self, options: &LiquidOptions) -> Result<Vec<Box<Renderable>>> {
        let mut renderables = Vec::new();
        while let Some(element) = self.next()? {
            renderables.push(parse_element(element, self.iter, options)?);
        }
        Ok(renderables)
    }

    pub fn to_string(&mut self) -> Result<String> {
        let mut s = String::new();
        while let Some(element) = self.next()? {
            s.push_str(element.as_str());
        } 
        Ok(s)
    }
}

/// An interface to parse tokens inside Tags without exposing the Pair structures
// TODO maybe TagTokens wrapping an iterator over tokens instead would be better.
// That way, users wouldn't need to constantly move_next and unwrap before parsing the token.
pub struct TagTokens<'a>{
    iter: &'a mut Iterator<Item = Pair<'a>>
}

impl<'a> TagTokens<'a> {
    fn new(tokens: &'a mut Iterator<Item = Pair<'a>>) -> Self {
        TagTokens {
            iter: tokens,
        }
    }


    fn expect_something(&mut self) -> Result<Pair<'a>> {
        match self.iter.next() {
            Some(pair) => Ok(pair),
            None => panic!("Error handling is not implemented. Expected something."),
        }
    }

    // TODO other tokens such as ranges, comparisons, assignments, ...

    // Maybe create an expect_symbol(&str) where &str is a known symbol in grammar?
    pub fn expect_assignment_operator(&mut self) -> Result<()> {
        let token = self.expect_something()?;
        if token.as_rule() != Rule::AssignmentOperator {
            return panic!(
                "Error handling is not implemented. Expected '='. {}",
                token.as_str()
            );
        }

        Ok(())
    }

    // TODO try to find a better approach
    // Values, Variables, Identifiers and Literals are caught by the Filterchain rule
    // The next methods unwrap the filterchain in order to obtain the desired rule

    fn unwrap_filter_chain(&mut self) -> Result<Pair<'a>> {
        let token = self.expect_something()?;

        if token.as_rule() != Rule::FilterChain {
            return panic!("Error handling is not implemented. Expected Filterchain.");
        }

        Ok(token)
    }

    fn unwrap_value(&mut self) -> Result<Pair<'a>> {
        let filterchain = self.unwrap_filter_chain()?;

        let mut chain = filterchain.into_inner();
        let value = chain.next().expect("Unwrapping value out of Filterchain.");
        if chain.next().is_some() {
            // There are filters: it can't be a value
            return panic!("Error handling is not implemented. Expected value.");
        }

        Ok(value)
    }

    fn unwrap_variable(&mut self) -> Result<Pair<'a>> {
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

    fn unwrap_identifier(&mut self) -> Result<Pair<'a>> {
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

    fn unwrap_literal(&mut self) -> Result<Pair<'a>> {
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

    pub fn expect_filter_chain(&mut self) -> Result<FilterChain> {
        let filterchain = self.unwrap_filter_chain()?;

        Ok(parse_filter_chain(filterchain))
    }

    pub fn expect_value(&mut self) -> Result<Expression> {
        let value = self.unwrap_value()?;

        Ok(parse_value(value))
    }

    pub fn expect_variable(&mut self) -> Result<Variable> {
        let variable = self.unwrap_variable()?;

        Ok(parse_variable(variable))
    }

    pub fn expect_identifier(&mut self) -> Result<&'a str> {
        let identifier = self.unwrap_identifier()?;

        Ok(identifier.as_str())
    }

    pub fn expect_literal(&mut self) -> Result<Value> {
        let literal = self.unwrap_literal()?;

        Ok(Value::scalar(parse_literal(literal)))
    }
}

#[cfg(test)]
mod test_parse_output {
    use super::*;

    // TODO tests
}
