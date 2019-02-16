use liquid_compiler::Filter;
use liquid_derive::*;
use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_value::Value;

/// Removes all whitespace (tabs, spaces, and newlines) from both the left and right side of a
/// string.
///
/// It does not affect spaces between words.  Note that while this works for the case of tabs,
/// spaces, and newlines, it also removes any other codepoints defined by the Unicode Derived Core
/// Property `White_Space` (per [rust
/// documentation](https://doc.rust-lang.org/std/primitive.str.html#method.trim_left).
#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "strip",
    description = "Removes all whitespace (tabs, spaces, and newlines) from both the left and right side of a string.",
    parsed(StripFilter)
)]
pub struct Strip;

#[derive(Debug, Default, Display_filter)]
#[name = "strip"]
struct StripFilter;

impl Filter for StripFilter {
    fn evaluate(&self, input: &Value, _context: &Context) -> Result<Value> {
        let input = input.to_str();
        Ok(Value::scalar(input.trim().to_owned()))
    }
}

/// Removes all whitespaces (tabs, spaces, and newlines) from the beginning of a string.
///
/// The filter does not affect spaces between words.  Note that while this works for the case of
/// tabs, spaces, and newlines, it also removes any other codepoints defined by the Unicode Derived
/// Core Property `White_Space` (per [rust
/// documentation](https://doc.rust-lang.org/std/primitive.str.html#method.trim_left).
#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "lstrip",
    description = "Removes all whitespaces (tabs, spaces, and newlines) from the beginning of a string.",
    parsed(LstripFilter)
)]
pub struct Lstrip;

#[derive(Debug, Default, Display_filter)]
#[name = "lstrip"]
struct LstripFilter;

impl Filter for LstripFilter {
    fn evaluate(&self, input: &Value, _context: &Context) -> Result<Value> {
        let input = input.to_str();
        Ok(Value::scalar(input.trim_left().to_owned()))
    }
}

/// Removes all whitespace (tabs, spaces, and newlines) from the right side of a string.
///
/// The filter does not affect spaces between words.  Note that while this works for the case of
/// tabs, spaces, and newlines, it also removes any other codepoints defined by the Unicode Derived
/// Core Property `White_Space` (per [rust
/// documentation](https://doc.rust-lang.org/std/primitive.str.html#method.trim_left).
#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "rstrip",
    description = "Removes all whitespace (tabs, spaces, and newlines) from the right side of a string.",
    parsed(RstripFilter)
)]
pub struct Rstrip;

#[derive(Debug, Default, Display_filter)]
#[name = "rstrip"]
struct RstripFilter;

impl Filter for RstripFilter {
    fn evaluate(&self, input: &Value, _context: &Context) -> Result<Value> {
        let input = input.to_str();
        Ok(Value::scalar(input.trim_right().to_owned()))
    }
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "strip_newlines",
    description = "Removes any newline characters (line breaks) from a string.",
    parsed(StripNewlinesFilter)
)]
pub struct StripNewlines;

#[derive(Debug, Default, Display_filter)]
#[name = "strip_newlines"]
struct StripNewlinesFilter;

impl Filter for StripNewlinesFilter {
    fn evaluate(&self, input: &Value, _context: &Context) -> Result<Value> {
        let input = input.to_str();
        Ok(Value::scalar(
            input
                .chars()
                .filter(|c| *c != '\n' && *c != '\r')
                .collect::<String>(),
        ))
    }
}
