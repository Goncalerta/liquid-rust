use std::io::Write;

use itertools;
use liquid_error::{Result, ResultLiquidChainExt, ResultLiquidExt};

use compiler::LiquidOptions;
use compiler::TagToken;
use compiler::TagTokenIter;
use interpreter::Context;
use interpreter::Expression;
use interpreter::Renderable;

#[derive(Clone, Debug)]
struct Cycle {
    name: String,
    values: Vec<Expression>,
}

impl Cycle {
    fn trace(&self) -> String {
        format!(
            "{{% cycle {} %}}",
            itertools::join(self.values.iter(), ", ")
        )
    }
}

impl Renderable for Cycle {
    fn render_to(&self, writer: &mut Write, context: &mut Context) -> Result<()> {
        let value = context
            .cycles()
            .cycle_element(&self.name, &self.values)
            .trace_with(|| self.trace())?;
        write!(writer, "{}", value).chain("Failed to render")?;
        Ok(())
    }
}

/// Internal implementation of cycle, to allow easier testing.
fn parse_cycle(mut arguments: TagTokenIter, _options: &LiquidOptions) -> Result<Cycle> {
    let mut name = String::new();
    let mut values = Vec::new();

    let first = arguments.expect_next("Identifier or value expected")?;
    let second = arguments.next();
    match second.as_ref().map(TagToken::as_str) {
        Some(":") => {
            // the first argument is the name of the cycle block
            // Using as_str directly might not be safe: unexpected tokens will be accepted
            name = first
                .expect_identifier()
                .map_err(TagToken::raise_error)?
                .to_string();
        }
        Some(",") | None => {
            // first argument is the first item in the cycle
            values.push(first.expect_value().map_err(TagToken::raise_error)?);
        }
        Some(_) => {
            return Err(second
                .expect("is some")
                .raise_custom_error("\":\" or \",\" expected."))
        }
    }

    loop {
        match arguments.next() {
            Some(a) => {
                values.push(a.expect_value().map_err(TagToken::raise_error)?);
            }
            None => break,
        }
        let next = arguments.next(); 
        match next.as_ref().map(TagToken::as_str) {
            Some(",") => {}
            None => break,
            Some(_) => return Err(next
                .expect("is some")
                .raise_custom_error("\",\" expected."))
        }
    }

    if name.is_empty() {
        name = itertools::join(values.iter(), "-");
    }

    Ok(Cycle { name, values })
}

pub fn cycle_tag(
    _tag_name: &str,
    arguments: TagTokenIter,
    options: &LiquidOptions,
) -> Result<Box<Renderable>> {
    parse_cycle(arguments, options).map(|opt| Box::new(opt) as Box<Renderable>)
}

#[cfg(test)]
mod test {
    // use super::*;
    // use compiler;
    // use interpreter;
    // use value::Value;

    // fn options() -> LiquidOptions {
    //     let mut options = LiquidOptions::default();
    //     options
    //         .tags
    //         .insert("cycle", (cycle_tag as compiler::FnParseTag).into());
    //     options
    // }

    // #[test]
    // fn unnamed_cycle_gets_a_name() {
    //     let tokens = vec![
    //         Token::Identifier("this".to_owned()),
    //         Token::Comma,
    //         Token::StringLiteral("cycle".to_owned()),
    //         Token::Comma,
    //         Token::Identifier("has".to_owned()),
    //         Token::Comma,
    //         Token::Identifier("no".to_owned()),
    //         Token::Comma,
    //         Token::Identifier("name".to_owned()),
    //     ];
    //     let options = LiquidOptions::default();
    //     let cycle = parse_cycle(&tokens[..], &options).unwrap();
    //     assert!(!cycle.name.is_empty());
    // }

    // #[test]
    // fn named_values_are_independent() {
    //     let text = concat!(
    //         "{% cycle 'a': 'one', 'two', 'three' %}\n",
    //         "{% cycle 'a': 'one', 'two', 'three' %}\n",
    //         "{% cycle 'b': 'one', 'two', 'three' %}\n",
    //         "{% cycle 'b': 'one', 'two', 'three' %}\n"
    //     ).to_owned();
    //     let tokens = compiler::tokenize(&text).unwrap();
    //     let template = compiler::parse(&tokens, &options())
    //         .map(interpreter::Template::new)
    //         .unwrap();

    //     let mut context = Context::new();
    //     let output = template.render(&mut context);

    //     assert_eq!(output.unwrap(), "one\ntwo\none\ntwo\n");
    // }

    // #[test]
    // fn values_are_cycled() {
    //     let text = concat!(
    //         "{% cycle 'one', 'two', 'three' %}\n",
    //         "{% cycle 'one', 'two', 'three' %}\n",
    //         "{% cycle 'one', 'two', 'three' %}\n",
    //         "{% cycle 'one', 'two', 'three' %}\n"
    //     ).to_owned();
    //     let tokens = compiler::tokenize(&text).unwrap();
    //     let template = compiler::parse(&tokens, &options())
    //         .map(interpreter::Template::new)
    //         .unwrap();

    //     let mut context = Context::new();
    //     let output = template.render(&mut context);

    //     assert_eq!(output.unwrap(), "one\ntwo\nthree\none\n");
    // }

    // #[test]
    // fn values_can_be_variables() {
    //     let text = concat!(
    //         "{% cycle alpha, beta, gamma %}\n",
    //         "{% cycle alpha, beta, gamma %}\n",
    //         "{% cycle alpha, beta, gamma %}\n",
    //         "{% cycle alpha, beta, gamma %}\n"
    //     ).to_owned();
    //     let tokens = compiler::tokenize(&text).unwrap();
    //     let template = compiler::parse(&tokens, &options())
    //         .map(interpreter::Template::new)
    //         .unwrap();

    //     let mut context = Context::new();
    //     context.stack_mut().set_global("alpha", Value::scalar(1f64));
    //     context.stack_mut().set_global("beta", Value::scalar(2f64));
    //     context.stack_mut().set_global("gamma", Value::scalar(3f64));

    //     let output = template.render(&mut context);

    //     assert_eq!(output.unwrap(), "1\n2\n3\n1\n");
    // }

    // #[test]
    // fn bad_cycle_indices_dont_crash() {
    //     // note the pair of cycle tags with the same name but a differing
    //     // number of elements
    //     let text = concat!("{% cycle c: 1, 2 %}\n", "{% cycle c: 1 %}\n").to_owned();

    //     let tokens = compiler::tokenize(&text).unwrap();
    //     let options = options();
    //     let template = compiler::parse(&tokens, &options)
    //         .map(interpreter::Template::new)
    //         .unwrap();
    //     let output = template.render(&mut Default::default());
    //     assert!(output.is_err());
    // }
}
