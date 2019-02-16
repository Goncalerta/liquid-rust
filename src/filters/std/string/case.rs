use liquid_compiler::Filter;
use liquid_derive::*;
use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_value::Value;

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "downcase",
    description = "Makes each character in a string downcase.",
    parsed(DowncaseFilter)
)]
pub struct Downcase;

#[derive(Debug, Default, Display_filter)]
#[name = "downcase"]
struct DowncaseFilter;

impl Filter for DowncaseFilter {
    fn evaluate(&self, input: &Value, _context: &Context) -> Result<Value> {
        let s = input.to_str();
        Ok(Value::scalar(s.to_lowercase()))
    }
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "upcase",
    description = "Makes each character in a string uppercase.",
    parsed(UpcaseFilter)
)]
pub struct Upcase;

#[derive(Debug, Default, Display_filter)]
#[name = "upcase"]
struct UpcaseFilter;

impl Filter for UpcaseFilter {
    fn evaluate(&self, input: &Value, _context: &Context) -> Result<Value> {
        let s = input.to_str();
        Ok(Value::scalar(s.to_uppercase()))
    }
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "capitalize",
    description = "Makes the first character of a string capitalized.",
    parsed(CapitalizeFilter)
)]
pub struct Capitalize;

#[derive(Debug, Default, Display_filter)]
#[name = "capitalize"]
struct CapitalizeFilter;

impl Filter for CapitalizeFilter {
    fn evaluate(&self, input: &Value, _context: &Context) -> Result<Value> {
        let s = input.to_str().to_owned();
        let mut chars = s.chars();
        let capitalized = match chars.next() {
            Some(first_char) => first_char.to_uppercase().chain(chars).collect(),
            None => String::new(),
        };

        Ok(Value::scalar(capitalized))
    }
}
