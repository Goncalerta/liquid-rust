use std::io::Write;

use itertools;
use liquid_error::{Error, Result, ResultLiquidExt};
use liquid_value::Value;

use compiler::LiquidOptions;
use compiler::TagToken;
use compiler::TagBlock;
use compiler::BlockElement;
use interpreter::Context;
use interpreter::Expression;
use interpreter::Renderable;
use interpreter::Template;

#[derive(Debug)]
struct CaseOption {
    args: Vec<Expression>,
    template: Template,
}

impl CaseOption {
    fn new(args: Vec<Expression>, template: Template) -> CaseOption {
        CaseOption { args, template }
    }

    fn evaluate(&self, value: &Value, context: &Context) -> Result<bool> {
        for a in &self.args {
            let v = a.evaluate(context)?;
            if v == *value {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn trace(&self) -> String {
        format!("{{% when {} %}}", itertools::join(self.args.iter(), " or "))
    }
}

#[derive(Debug)]
struct Case {
    target: Expression,
    cases: Vec<CaseOption>,
    else_block: Option<Template>,
}

impl Case {
    fn trace(&self) -> String {
        format!("{{% case {} %}}", self.target)
    }
}

impl Renderable for Case {
    fn render_to(&self, writer: &mut Write, context: &mut Context) -> Result<()> {
        let value = self.target.evaluate(context)?;
        for case in &self.cases {
            if case.evaluate(&value, context)? {
                return case
                    .template
                    .render_to(writer, context)
                    .trace_with(|| case.trace())
                    .trace_with(|| self.trace())
                    .context_with(|| (self.target.to_string(), value.to_string()));
            }
        }

        if let Some(ref t) = self.else_block {
            return t
                .render_to(writer, context)
                .trace("{{% else %}}")
                .trace_with(|| self.trace())
                .context_with(|| (self.target.to_string(), value.to_string()));
        }

        Ok(())
    }
}

fn parse_condition(arguments: &mut Iterator<Item = TagToken>) -> Result<Vec<Expression>> {
    let mut values = Vec::new();

    let first_value = arguments.next().unwrap_or_else(|| panic!("Errors not implemented. Token expected."));
    let first_value = first_value.expect_value().map_err(TagToken::raise_error)?;
    values.push(first_value);

    while let Some(token) = arguments.next() {
        if token.as_str() != "or" {
            panic!("Errors not implemented. Unexpected token.");
        }
        let value = arguments.next().unwrap_or_else(|| panic!("Errors not implemented. Token expected."));
        let value = value.expect_value().map_err(TagToken::raise_error)?;
        values.push(value);
    }

    Ok(values)     
}

pub fn case_block(
    _tag_name: &str,
    arguments: &mut Iterator<Item = TagToken>,
    tokens: &mut TagBlock,
    options: &LiquidOptions,
) -> Result<Box<Renderable>> {

    let target = arguments.next().unwrap_or_else(|| panic!("Errors not implemented. Token expected."));
    let target = target.expect_value().map_err(TagToken::raise_error)?;

    let mut cases = Vec::new();
    let mut else_block = None;
    let mut current_block = Vec::new();
    let mut current_condition = None;

    while let Some(element) = tokens.next()? {
        match element {
            BlockElement::Tag(mut tag) => {
                match tag.name() {
                    "when" => {
                        if let Some(condition) = current_condition {
                            cases.push(CaseOption::new(condition, Template::new(current_block)));
                        }
                        current_block = Vec::new();
                        current_condition = Some(parse_condition(tag.tokens())?);
                    },
                    "else" => else_block = Some(tokens.parse(options)?),
                    _ => current_block.push(tag.parse(tokens, options)?),
                }
            },
            element => current_block.push(element.parse(tokens, options)?),
        }
    }

    let else_block = else_block.map(Template::new);

    Ok(Box::new(Case {
        target,
        cases,
        else_block,
    }))
}

#[cfg(test)]
mod test {
    // use super::*;
    // use compiler;
    // use interpreter;

    // fn options() -> LiquidOptions {
    //     let mut options = LiquidOptions::default();
    //     options
    //         .blocks
    //         .insert("case", (case_block as compiler::FnParseBlock).into());
    //     options
    // }

    // #[test]
    // fn test_case_block() {
    //     let text = concat!(
    //         "{% case x %}",
    //         "{% when 2 %}",
    //         "two",
    //         "{% when 3 or 4 %}",
    //         "three and a half",
    //         "{% else %}",
    //         "otherwise",
    //         "{% endcase %}"
    //     );
    //     let tokens = compiler::tokenize(text).unwrap();
    //     let options = options();
    //     let template = compiler::parse(&tokens, &options)
    //         .map(interpreter::Template::new)
    //         .unwrap();

    //     let mut context = Context::new();
    //     context.stack_mut().set_global("x", Value::scalar(2f64));
    //     assert_eq!(template.render(&mut context).unwrap(), "two");

    //     context.stack_mut().set_global("x", Value::scalar(3f64));
    //     assert_eq!(template.render(&mut context).unwrap(), "three and a half");

    //     context.stack_mut().set_global("x", Value::scalar(4f64));
    //     assert_eq!(template.render(&mut context).unwrap(), "three and a half");

    //     context.stack_mut().set_global("x", Value::scalar("nope"));
    //     assert_eq!(template.render(&mut context).unwrap(), "otherwise");
    // }

    // #[test]
    // fn test_no_matches_returns_empty_string() {
    //     let text = concat!(
    //         "{% case x %}",
    //         "{% when 2 %}",
    //         "two",
    //         "{% when 3 or 4 %}",
    //         "three and a half",
    //         "{% endcase %}"
    //     );
    //     let tokens = compiler::tokenize(text).unwrap();
    //     let options = options();
    //     let template = compiler::parse(&tokens, &options)
    //         .map(interpreter::Template::new)
    //         .unwrap();

    //     let mut context = Context::new();
    //     context.stack_mut().set_global("x", Value::scalar("nope"));
    //     assert_eq!(template.render(&mut context).unwrap(), "");
    // }

    // #[test]
    // fn multiple_else_blocks_is_an_error() {
    //     let text = concat!(
    //         "{% case x %}",
    //         "{% when 2 %}",
    //         "two",
    //         "{% else %}",
    //         "else #1",
    //         "{% else %}",
    //         "else # 2",
    //         "{% endcase %}"
    //     );
    //     let tokens = compiler::tokenize(text).unwrap();
    //     let options = options();
    //     let template = compiler::parse(&tokens, &options).map(interpreter::Template::new);
    //     assert!(template.is_err());
    // }
}
