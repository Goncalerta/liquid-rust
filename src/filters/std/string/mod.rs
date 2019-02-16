use liquid_compiler::{Filter, FilterParameters};
use liquid_derive::*;
use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_interpreter::Expression;
use liquid_value::Value;

pub mod case;
pub mod operate;
pub mod slice;
pub mod strip;

#[derive(Debug, FilterParameters)]
struct SplitArgs {
    #[parameter(
        description = "The separator between each element in the string.",
        arg_type = "str"
    )]
    pattern: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "split",
    description = "Divides an input string into an array using the argument as a separator.",
    parameters(SplitArgs),
    parsed(SplitFilter)
)]
pub struct Split;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "split"]
struct SplitFilter {
    #[parameters]
    args: SplitArgs,
}

impl Filter for SplitFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let input = input.to_str();

        // Split and construct resulting Array
        Ok(Value::Array(
            input
                .split(args.pattern.as_ref())
                .map(|s| Value::scalar(s.to_owned()))
                .collect(),
        ))
    }
}
