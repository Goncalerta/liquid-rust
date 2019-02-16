use liquid_compiler::{Filter, FilterParameters};
use liquid_derive::*;
use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_interpreter::Expression;
use liquid_value::Value;

#[derive(Debug, FilterParameters)]
struct ReplaceArgs {
    #[parameter(description = "The text to search.", arg_type = "str")]
    search: Expression,
    #[parameter(
        description = "The text to replace search results with. If not given, the filter will just delete search results.",
        arg_type = "str"
    )]
    replace: Option<Expression>,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "replace",
    description = "Replaces the occurrences of the `search` with `replace`. If `replace` is not given, just deletes occurrences of `search`.",
    parameters(ReplaceArgs),
    parsed(ReplaceFilter)
)]
pub struct Replace;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "replace"]
struct ReplaceFilter {
    #[parameters]
    args: ReplaceArgs,
}

impl Filter for ReplaceFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let input = input.to_str();

        let replace = args.replace.unwrap_or_else(|| "".into());

        Ok(Value::scalar(
            input.replace(args.search.as_ref(), replace.as_ref()),
        ))
    }
}

#[derive(Debug, FilterParameters)]
struct ReplaceFirstArgs {
    #[parameter(description = "The text to search.", arg_type = "str")]
    search: Expression,
    #[parameter(
        description = "The text to replace search result with. If not given, the filter will just delete search results«.",
        arg_type = "str"
    )]
    replace: Option<Expression>,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "replace_first",
    description = "Replaces the first occurrence of the `search` with `replace`. If `replace` is not given, just deletes the occurrence.",
    parameters(ReplaceFirstArgs),
    parsed(ReplaceFirstFilter)
)]
pub struct ReplaceFirst;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "replace_first"]
struct ReplaceFirstFilter {
    #[parameters]
    args: ReplaceFirstArgs,
}

impl Filter for ReplaceFirstFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let input = input.to_str();

        let search = args.search;
        let replace = args.replace.unwrap_or_else(|| "".into());

        {
            let tokens: Vec<&str> = input.splitn(2, search.as_ref()).collect();
            if tokens.len() == 2 {
                let result = [tokens[0], replace.as_ref(), tokens[1]].join("");
                return Ok(Value::scalar(result));
            }
        }
        Ok(Value::scalar(input.into_owned()))
    }
}

#[derive(Debug, FilterParameters)]
struct RemoveArgs {
    #[parameter(description = "The text to remove.", arg_type = "str")]
    search: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "remove",
    description = "Removes all occurrences of the given string.",
    parameters(RemoveArgs),
    parsed(RemoveFilter)
)]
pub struct Remove;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "remove"]
struct RemoveFilter {
    #[parameters]
    args: RemoveArgs,
}

impl Filter for RemoveFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let input = input.to_str();

        Ok(Value::scalar(input.replace(args.search.as_ref(), "")))
    }
}

#[derive(Debug, FilterParameters)]
struct RemoveFirstArgs {
    #[parameter(description = "The text to remove.", arg_type = "str")]
    search: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "remove_first",
    description = "Removes the first occurrence of the given string.",
    parameters(RemoveFirstArgs),
    parsed(RemoveFirstFilter)
)]
pub struct RemoveFirst;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "remove_first"]
struct RemoveFirstFilter {
    #[parameters]
    args: RemoveFirstArgs,
}

impl Filter for RemoveFirstFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let input = input.to_str();

        Ok(Value::scalar(
            input.splitn(2, args.search.as_ref()).collect::<String>(),
        ))
    }
}

#[derive(Debug, FilterParameters)]
struct AppendArgs {
    #[parameter(description = "The string to append to the input.", arg_type = "str")]
    string: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "append",
    description = "Appends the given text to a string.",
    parameters(AppendArgs),
    parsed(AppendFilter)
)]
pub struct Append;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "append"]
struct AppendFilter {
    #[parameters]
    args: AppendArgs,
}

impl Filter for AppendFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let mut input = input.to_str().into_owned();
        input.push_str(args.string.as_ref());

        Ok(Value::scalar(input))
    }
}

#[derive(Debug, FilterParameters)]
struct PrependArgs {
    #[parameter(description = "The string to prepend to the input.", arg_type = "str")]
    string: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "prepend",
    description = "Prepends the given text to a string.",
    parameters(PrependArgs),
    parsed(PrependFilter)
)]
pub struct Prepend;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "prepend"]
struct PrependFilter {
    #[parameters]
    args: PrependArgs,
}

impl Filter for PrependFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let input = input.to_str();
        let mut string = args.string.into_owned();
        string.push_str(input.as_ref());

        Ok(Value::scalar(string))
    }
}
