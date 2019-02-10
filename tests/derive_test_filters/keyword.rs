extern crate liquid;
use liquid::compiler::{Filter, FilterParameters};
use liquid::derive::*;
use liquid::error::Result;
use liquid::interpreter::Context;
use liquid::interpreter::Expression;
use liquid::value::Value;

#[derive(Debug, FilterParameters)]
struct TestKeywordFilterParameters {
    #[parameter(
        description = "Optional keyword argument.",
        value = "str",
        mode = "keyword"
    )]
    optional: Option<Expression>,

    #[parameter(
        description = "Required keyword argument. Must be a boolean.",
        value = "bool",
        mode = "keyword"
    )]
    required: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "kw",
    description = "Filter to test keyword arguments.",
    parameters(TestKeywordFilterParameters),
    parsed(TestKeywordFilter)
)]
pub struct TestKeywordFilterParser;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "kw"]
pub struct TestKeywordFilter {
    #[parameters]
    args: TestKeywordFilterParameters,
}

impl Filter for TestKeywordFilter {
    fn evaluate(&self, _input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let required = args.required;

        let result = if let Some(optional) = args.optional {
            format!("<optional: {}; required: {}>", optional, required)
        } else {
            format!("<required: {}>", required)
        };

        Ok(Value::scalar(result))
    }
}
