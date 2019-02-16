use filters::invalid_argument;
use itertools;
use liquid_compiler::{Filter, FilterParameters};
use liquid_derive::*;
use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_interpreter::Expression;
use liquid_value::Value;
use std::cmp;
use unicode_segmentation::UnicodeSegmentation;

fn canonicalize_slice(
    slice_offset: isize,
    slice_length: isize,
    vec_length: usize,
) -> (usize, usize) {
    let vec_length = vec_length as isize;

    // Cap slice_offset
    let slice_offset = cmp::min(slice_offset, vec_length);
    // Reverse indexing
    let slice_offset = if slice_offset < 0 {
        slice_offset + vec_length
    } else {
        slice_offset
    };

    // Cap slice_length
    let slice_length = if slice_offset + slice_length > vec_length {
        vec_length - slice_offset
    } else {
        slice_length
    };

    (slice_offset as usize, slice_length as usize)
}

#[derive(Debug, FilterParameters)]
struct SliceArgs {
    #[parameter(description = "The offset of the slice.", arg_type = "integer")]
    offset: Expression,

    #[parameter(description = "The length of the slice.", arg_type = "integer")]
    length: Option<Expression>,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "slice",
    description = "Takes a slice of a given string or array.",
    parameters(SliceArgs),
    parsed(SliceFilter)
)]
pub struct Slice;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "slice"]
struct SliceFilter {
    #[parameters]
    args: SliceArgs,
}

impl Filter for SliceFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let offset = args.offset as isize;
        let length = args.length.unwrap_or(1) as isize;

        if length < 1 {
            return invalid_argument("length", "Positive number expected").into_err();
        }

        if let Value::Array(input) = input {
            let (offset, length) = canonicalize_slice(offset, length, input.len());
            Ok(Value::array(
                input.iter().skip(offset).take(length).cloned(),
            ))
        } else {
            let input = input.to_str();
            let (offset, length) = canonicalize_slice(offset, length, input.len());
            Ok(Value::scalar(
                input.chars().skip(offset).take(length).collect::<String>(),
            ))
        }
    }
}

#[derive(Debug, FilterParameters)]
struct TruncateArgs {
    #[parameter(
        description = "The maximum lenght of the string, after which it will be truncated.",
        arg_type = "integer"
    )]
    lenght: Option<Expression>,

    #[parameter(
        description = "The text appended to the end of the string if it is truncated. This text counts to the maximum lenght of the string. Defaults to \"...\".",
        arg_type = "str"
    )]
    ellipsis: Option<Expression>,
}

/// `truncate` shortens a string down to the number of characters passed as a parameter.
///
/// Note that this function operates on [grapheme
/// clusters](http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries) (or *user-perceived
/// character*), rather than Unicode code points.  Each grapheme cluster may be composed of more
/// than one Unicode code point, and does not necessarily correspond to rust's conception of a
/// character.
///
/// If the number of characters specified is less than the length of the string, an ellipsis
/// (`...`) is appended to the string and is included in the character count.
///
/// ## Custom ellipsis
///
/// `truncate` takes an optional second parameter that specifies the sequence of characters to be
/// appended to the truncated string. By default this is an ellipsis (`...`), but you can specify a
/// different sequence.
///
/// The length of the second parameter counts against the number of characters specified by the
/// first parameter. For example, if you want to truncate a string to exactly 10 characters, and
/// use a 3-character ellipsis, use 13 for the first parameter of `truncate`, since the ellipsis
/// counts as 3 characters.
///
/// ## No ellipsis
///
/// You can truncate to the exact number of characters specified by the first parameter and show no
/// trailing characters by passing a blank string as the second parameter.
#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "truncate",
    description = "Shortens a string down to the number of characters passed as a parameter.",
    parameters(TruncateArgs),
    parsed(TruncateFilter)
)]
pub struct Truncate;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "truncate"]
struct TruncateFilter {
    #[parameters]
    args: TruncateArgs,
}

impl Filter for TruncateFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let lenght = args.lenght.unwrap_or(50) as usize;

        let truncate_string = args.ellipsis.unwrap_or_else(|| "...".into());

        let l = cmp::max(lenght - truncate_string.len(), 0);

        let input_string = input.to_str();

        let result = if lenght < input_string.len() {
            let result = UnicodeSegmentation::graphemes(input_string.as_ref(), true)
                .take(l)
                .collect::<Vec<&str>>()
                .join("")
                .to_string()
                + truncate_string.as_ref();
            Value::scalar(result)
        } else {
            input.clone()
        };
        Ok(result)
    }
}

#[derive(Debug, FilterParameters)]
struct TruncateWordsArgs {
    #[parameter(
        description = "The maximum number of words, after which the string will be truncated.",
        arg_type = "integer"
    )]
    lenght: Option<Expression>,

    #[parameter(
        description = "The text appended to the end of the string if it is truncated. This text counts to the maximum word-count of the string. Defaults to \"...\".",
        arg_type = "str"
    )]
    ellipsis: Option<Expression>,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "truncatewords",
    description = "Shortens a string down to the number of characters passed as a parameter.",
    parameters(TruncateWordsArgs),
    parsed(TruncateWordsFilter)
)]
pub struct TruncateWords;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "truncate"]
struct TruncateWordsFilter {
    #[parameters]
    args: TruncateWordsArgs,
}

impl Filter for TruncateWordsFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        let words = args.lenght.unwrap_or(50) as usize;

        let truncate_string = args.ellipsis.unwrap_or_else(|| "...".into());

        let l = cmp::max(words, 0);

        let input_string = input.to_str();

        let word_list: Vec<&str> = input_string.split(' ').collect();
        let result = if words < word_list.len() {
            let result = itertools::join(word_list.iter().take(l), " ") + truncate_string.as_ref();
            Value::scalar(result)
        } else {
            input.clone()
        };
        Ok(result)
    }
}
