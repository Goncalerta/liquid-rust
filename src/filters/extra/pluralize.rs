use filters::invalid_input;
use liquid_compiler::{Filter, FilterParameters};
use liquid_derive::*;
use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_interpreter::Expression;
use liquid_value::{Scalar, Value};

// shopify-specific

#[derive(Debug, FilterParameters)]
struct PluralizeArgs {
    #[parameter(description = "The singular version of the string.")]
    singular: Expression,
    #[parameter(description = "The plural version of the string.")]
    plural: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "pluralize",
    description = "Outputs the singular or plural version of a string based on the value of the input.",
    parameters(PluralizeArgs),
    parsed(PluralizeFilter)
)]
pub struct Pluralize;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "pluralize"]
struct PluralizeFilter {
    #[parameters]
    args: PluralizeArgs,
}

impl Filter for PluralizeFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let n = input
            .as_scalar()
            .and_then(Scalar::to_integer)
            .ok_or_else(|| invalid_input("Whole number expected"))?;

        if (n as isize) == 1 {
            Ok(args.singular.clone())
        } else {
            Ok(args.plural.clone())
        }
    }
}
