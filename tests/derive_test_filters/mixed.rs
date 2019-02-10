extern crate liquid;
use liquid::compiler::{Filter, FilterParameters};
use liquid::error::{Result};
use liquid::interpreter::Context;
use liquid::interpreter::Expression;
use liquid::value::Value;
use liquid::derive::*;

// Colision with FilterParameters' evaluated struct.
#[allow(dead_code)]
struct EvaluatedTestMixedFilterParameters;

#[derive(Debug, FilterParameters)]
#[evaluated(TestMixedFilterParametersEvaluated)]
struct TestMixedFilterParameters {
    #[parameter(description = "1", value = "integer", mode = "keyword")]
    a: Option<Expression>,

    #[parameter(description = "2", value = "bool")]
    b: Expression,

    #[parameter(description = "3", value = "float", mode = "keyword")]
    c: Option<Expression>,

    #[parameter(description = "4", value = "date")]
    d: Expression,

    #[parameter(description = "5", value = "str")]
    e: Option<Expression>,

    #[parameter(rename="type", description = "6", value = "any", mode = "keyword")]
    f: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "mix",
    description = "Mix it all together.",
    parameters(TestMixedFilterParameters),
    parsed(TestMixedFilter)
)]
pub struct TestMixedFilterParser;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "mix"]
pub struct TestMixedFilter {
    #[parameters]
    args: TestMixedFilterParameters,
}

impl Filter for TestMixedFilter {
    fn evaluate(&self, _input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;
        
        let a = args.a.map(|i| i.to_string()).unwrap_or("None".to_string());
        let b = args.b.to_string();
        let c = args.c.map(|i| i.to_string()).unwrap_or("None".to_string());
        let d = args.d.to_string();
        let e = args.e.map(|i| i.to_string()).unwrap_or("None".to_string());
        let f = args.f.to_str();

        let result = format!("<a: {}; b: {}; c: {}, d: {}, e: {}, type: {}>", a,b,c,d,e,f);

        Ok(Value::scalar(result))
    }
}