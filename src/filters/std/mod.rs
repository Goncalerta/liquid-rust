use liquid_compiler::{Filter, FilterParameters};
use liquid_derive::*;
use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_interpreter::Expression;
use liquid_value::Value;

mod array;
mod date;
mod html;
mod math;
mod string;
mod url;

pub use self::array::{Compact, Concat, First, Join, Last, Map, Reverse, Sort, SortNatural, Uniq};
pub use self::date::Date;
pub use self::html::{Escape, EscapeOnce, NewlineToBr, StripHtml};
pub use self::math::{
    Abs, AtLeast, AtMost, Ceil, DividedBy, Floor, Minus, Modulo, Plus, Round, Times,
};
pub use self::string::case::{Capitalize, Downcase, Upcase};
pub use self::string::operate::{Append, Prepend, Remove, RemoveFirst, Replace, ReplaceFirst};
pub use self::string::slice::{Slice, Truncate, TruncateWords};
pub use self::string::strip::{Lstrip, Rstrip, Strip, StripNewlines};
pub use self::string::Split;
pub use self::url::{UrlDecode, UrlEncode};

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "size",
    description = "Returns the size of the input. For an array or object this is the number of elemets. For other values it's the lenght of its string representation.",
    parsed(SizeFilter)
)]
pub struct Size;

#[derive(Debug, Default, Display_filter)]
#[name = "size"]
struct SizeFilter;

impl Filter for SizeFilter {
    fn evaluate(&self, input: &Value, _context: &Context) -> Result<Value> {
        match *input {
            Value::Scalar(ref x) => Ok(Value::scalar(x.to_str().len() as i32)),
            Value::Array(ref x) => Ok(Value::scalar(x.len() as i32)),
            Value::Object(ref x) => Ok(Value::scalar(x.len() as i32)),
            _ => Ok(Value::scalar(0i32)),
        }
    }
}

#[derive(Debug, FilterParameters)]
struct DefaultArgs {
    #[parameter(description = "The default value.")]
    default: Expression,
}

#[derive(Clone, ParseFilter, FilterReflection)]
#[filter(
    name = "default",
    description = "Sets a default value for the given input.",
    parameters(DefaultArgs),
    parsed(DefaultFilter)
)]
pub struct Default;

#[derive(Debug, FromFilterParameters, Display_filter)]
#[name = "default"]
struct DefaultFilter {
    #[parameters]
    args: DefaultArgs,
}

impl Filter for DefaultFilter {
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value> {
        let args = self.args.evaluate(context)?;

        if input.is_default() {
            Ok(args.default.clone())
        } else {
            Ok(input.clone())
        }
    }
}

// #[cfg(test)]
// mod tests {

//     use super::*;
//     use value::Object;

//     macro_rules! unit {
//         ($a:ident, $b:expr) => {{
//             unit!($a, $b, &[])
//         }};
//         ($a:ident, $b:expr, $c:expr) => {{
//             $a(&$b, $c).unwrap()
//         }};
//     }

//     macro_rules! failed {
//         ($a:ident, $b:expr) => {{
//             failed!($a, $b, &[])
//         }};
//         ($a:ident, $b:expr, $c:expr) => {{
//             $a(&$b, $c).unwrap_err()
//         }};
//     }

//     macro_rules! tos {
//         ($a:expr) => {{
//             Value::scalar($a.to_owned())
//         }};
//     }

//     #[test]
//     fn unit_append() {
//         assert_eq!(unit!(append, tos!("sam"), &[tos!("son")]), tos!("samson"));
//     }

//     #[test]
//     fn unit_concat_nothing() {
//         let input = Value::Array(vec![Value::scalar(1f64), Value::scalar(2f64)]);
//         let args = &[Value::Array(vec![])];
//         let result = Value::Array(vec![Value::scalar(1f64), Value::scalar(2f64)]);
//         assert_eq!(unit!(concat, input, args), result);
//     }

//     #[test]
//     fn unit_concat_something() {
//         let input = Value::Array(vec![Value::scalar(1f64), Value::scalar(2f64)]);
//         let args = &[Value::Array(vec![Value::scalar(3f64), Value::scalar(4f64)])];
//         let result = Value::Array(vec![
//             Value::scalar(1f64),
//             Value::scalar(2f64),
//             Value::scalar(3f64),
//             Value::scalar(4f64),
//         ]);
//         assert_eq!(unit!(concat, input, args), result);
//     }

//     #[test]
//     fn unit_concat_mixed() {
//         let input = Value::Array(vec![Value::scalar(1f64), Value::scalar(2f64)]);
//         let args = &[Value::Array(vec![Value::scalar(3f64), Value::scalar("a")])];
//         let result = Value::Array(vec![
//             Value::scalar(1f64),
//             Value::scalar(2f64),
//             Value::scalar(3f64),
//             Value::scalar("a"),
//         ]);
//         assert_eq!(unit!(concat, input, args), result);
//     }

//     #[test]
//     fn unit_concat_wrong_type() {
//         let input = Value::Array(vec![Value::scalar(1f64), Value::scalar(2f64)]);
//         let args = &[Value::scalar(1f64)];
//         failed!(concat, input, args);
//     }

//     #[test]
//     fn unit_concat_no_args() {
//         let input = Value::Array(vec![Value::scalar(1f64), Value::scalar(2f64)]);
//         let args = &[];
//         failed!(concat, input, args);
//     }

//     #[test]
//     fn unit_concat_extra_args() {
//         let input = Value::Array(vec![Value::scalar(1f64), Value::scalar(2f64)]);
//         let args = &[
//             Value::Array(vec![Value::scalar(3f64), Value::scalar("a")]),
//             Value::scalar(2f64),
//         ];
//         failed!(concat, input, args);
//     }

//     #[test]
//     fn unit_capitalize() {
//         assert_eq!(unit!(capitalize, tos!("abc")), tos!("Abc"));
//         assert_eq!(
//             unit!(capitalize, tos!("hello world 21")),
//             tos!("Hello world 21")
//         );

//         // sure that Umlauts work
//         assert_eq!(
//             unit!(capitalize, tos!("über ètat, y̆es?")),
//             tos!("Über ètat, y̆es?")
//         );

//         // Weird UTF-8 White space is kept – this is a no-break whitespace!
//         assert_eq!(
//             unit!(capitalize, tos!("hello world​")),
//             tos!("Hello world​")
//         );

//         // The uppercase version of some character are more than one character long
//         assert_eq!(unit!(capitalize, tos!("ßß")), tos!("SSß"));
//     }

//     #[test]
//     fn unit_ceil() {
//         assert_eq!(unit!(ceil, Value::scalar(1.1f64), &[]), Value::scalar(2f64));
//         assert_eq!(unit!(ceil, Value::scalar(1f64), &[]), Value::scalar(1f64));
//         assert!(ceil(&Value::scalar(true), &[]).is_err());
//     }

//     #[test]
//     fn unit_downcase() {
//         assert_eq!(unit!(downcase, tos!("Abc")), tos!("abc"));
//         assert_eq!(
//             unit!(downcase, tos!("Hello World 21")),
//             tos!("hello world 21")
//         );
//     }

//     #[test]
//     fn unit_first() {
//         assert_eq!(
//             unit!(
//                 first,
//                 Value::Array(vec![
//                     Value::scalar(0f64),
//                     Value::scalar(1f64),
//                     Value::scalar(2f64),
//                     Value::scalar(3f64),
//                     Value::scalar(4f64),
//                 ])
//             ),
//             Value::scalar(0f64)
//         );
//         assert_eq!(
//             unit!(first, Value::Array(vec![tos!("test"), tos!("two")])),
//             tos!("test")
//         );
//         assert_eq!(unit!(first, Value::Array(vec![])), tos!(""));
//     }

//     #[test]
//     fn unit_floor() {
//         assert_eq!(
//             unit!(floor, Value::scalar(1.1f64), &[]),
//             Value::scalar(1f64)
//         );
//         assert_eq!(unit!(floor, Value::scalar(1f64), &[]), Value::scalar(1f64));
//         assert!(floor(&Value::scalar(true), &[]).is_err());
//     }

//     #[test]
//     fn unit_join() {
//         let input = Value::Array(vec![tos!("a"), tos!("b"), tos!("c")]);
//         let args = &[tos!(",")];
//         let result = join(&input, args);
//         assert_eq!(result.unwrap(), tos!("a,b,c"));
//     }

//     #[test]
//     fn unit_join_bad_input() {
//         let input = tos!("a");
//         let args = &[tos!(",")];
//         let result = join(&input, args);
//         assert!(result.is_err());
//     }

//     #[test]
//     fn unit_join_bad_join_string() {
//         let input = Value::Array(vec![tos!("a"), tos!("b"), tos!("c")]);
//         let args = &[Value::scalar(1f64)];
//         let result = join(&input, args);
//         assert_eq!(result.unwrap(), tos!("a1b1c"));
//     }

//     #[test]
//     fn unit_join_no_args() {
//         let input = Value::Array(vec![tos!("a"), tos!("b"), tos!("c")]);
//         let args = &[];
//         let result = join(&input, args);
//         assert_eq!(result.unwrap(), tos!("a b c"));
//     }

//     #[test]
//     fn unit_join_non_string_element() {
//         let input = Value::Array(vec![tos!("a"), Value::scalar(1f64), tos!("c")]);
//         let args = &[tos!(",")];
//         let result = join(&input, args);
//         assert_eq!(result.unwrap(), tos!("a,1,c"));
//     }

//     #[test]
//     fn unit_sort() {
//         let input = &Value::Array(vec![tos!("Z"), tos!("b"), tos!("c"), tos!("a")]);
//         let args = &[];
//         let desired_result = Value::Array(vec![tos!("Z"), tos!("a"), tos!("b"), tos!("c")]);
//         assert_eq!(unit!(sort, input, args), desired_result);
//     }

//     #[test]
//     fn unit_sort_natural() {
//         let input = &Value::Array(vec![tos!("Z"), tos!("b"), tos!("c"), tos!("a")]);
//         let args = &[];
//         let desired_result = Value::Array(vec![tos!("a"), tos!("b"), tos!("c"), tos!("Z")]);
//         assert_eq!(unit!(sort_natural, input, args), desired_result);
//     }

//     #[test]
//     fn unit_last() {
//         assert_eq!(
//             unit!(
//                 last,
//                 Value::Array(vec![
//                     Value::scalar(0f64),
//                     Value::scalar(1f64),
//                     Value::scalar(2f64),
//                     Value::scalar(3f64),
//                     Value::scalar(4f64),
//                 ])
//             ),
//             Value::scalar(4f64)
//         );
//         assert_eq!(
//             unit!(last, Value::Array(vec![tos!("test"), tos!("last")])),
//             tos!("last")
//         );
//         assert_eq!(unit!(last, Value::Array(vec![])), tos!(""));
//     }

//     #[test]
//     fn unit_lstrip() {
//         let input = &tos!(" 	 \n \r test");
//         let args = &[];
//         let desired_result = tos!("test");
//         assert_eq!(unit!(lstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_lstrip_non_string() {
//         let input = &Value::scalar(0f64);
//         let args = &[];
//         let desired_result = tos!("0");
//         assert_eq!(unit!(lstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_lstrip_one_argument() {
//         let input = &tos!(" 	 \n \r test");
//         let args = &[Value::scalar(0f64)];
//         failed!(lstrip, input, args);
//     }

//     #[test]
//     fn unit_lstrip_shopify_liquid() {
//         // One test from https://shopify.github.io/liquid/filters/lstrip/
//         let input = &tos!("          So much room for activities!          ");
//         let args = &[];
//         let desired_result = tos!("So much room for activities!          ");
//         assert_eq!(unit!(lstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_lstrip_trailing_sequence() {
//         let input = &tos!(" 	 \n \r test 	 \n \r ");
//         let args = &[];
//         let desired_result = tos!("test 	 \n \r ");
//         assert_eq!(unit!(lstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_lstrip_trailing_sequence_only() {
//         let input = &tos!("test 	 \n \r ");
//         let args = &[];
//         let desired_result = tos!("test 	 \n \r ");
//         assert_eq!(unit!(lstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_prepend() {
//         assert_eq!(
//             unit!(prepend, tos!("barbar"), &[tos!("foo")]),
//             tos!("foobarbar")
//         );
//     }

//     #[test]
//     fn unit_remove() {
//         assert_eq!(unit!(remove, tos!("barbar"), &[tos!("bar")]), tos!(""));
//         assert_eq!(unit!(remove, tos!("barbar"), &[tos!("")]), tos!("barbar"));
//         assert_eq!(unit!(remove, tos!("barbar"), &[tos!("barbar")]), tos!(""));
//         assert_eq!(unit!(remove, tos!("barbar"), &[tos!("a")]), tos!("brbr"));
//     }

//     #[test]
//     fn unit_remove_first() {
//         assert_eq!(
//             unit!(remove_first, tos!("barbar"), &[tos!("bar")]),
//             tos!("bar")
//         );
//         assert_eq!(
//             unit!(remove_first, tos!("barbar"), &[tos!("")]),
//             tos!("barbar")
//         );
//         assert_eq!(
//             unit!(remove_first, tos!("barbar"), &[tos!("barbar")]),
//             tos!("")
//         );
//         assert_eq!(
//             unit!(remove_first, tos!("barbar"), &[tos!("a")]),
//             tos!("brbar")
//         );
//     }

//     #[test]
//     fn unit_replace() {
//         assert_eq!(
//             unit!(replace, tos!("barbar"), &[tos!("bar"), tos!("foo")]),
//             tos!("foofoo")
//         );
//     }

//     #[test]
//     fn unit_replace_first() {
//         assert_eq!(
//             unit!(replace_first, tos!("barbar"), &[tos!("bar"), tos!("foo")]),
//             tos!("foobar")
//         );
//         assert_eq!(
//             unit!(replace_first, tos!("barxoxo"), &[tos!("xo"), tos!("foo")]),
//             tos!("barfooxo")
//         );
//         assert_eq!(
//             unit!(replace_first, tos!(""), &[tos!("bar"), tos!("foo")]),
//             tos!("")
//         );
//     }

//     #[test]
//     fn unit_reverse_apples_oranges_peaches_plums() {
//         // First example from https://shopify.github.io/liquid/filters/reverse/
//         let input = &Value::Array(vec![
//             tos!("apples"),
//             tos!("oranges"),
//             tos!("peaches"),
//             tos!("plums"),
//         ]);
//         let args = &[];
//         let desired_result = Value::Array(vec![
//             tos!("plums"),
//             tos!("peaches"),
//             tos!("oranges"),
//             tos!("apples"),
//         ]);
//         assert_eq!(unit!(reverse, input, args), desired_result);
//     }

//     #[test]
//     fn unit_reverse_array() {
//         let input = &Value::Array(vec![
//             Value::scalar(3f64),
//             Value::scalar(1f64),
//             Value::scalar(2f64),
//         ]);
//         let args = &[];
//         let desired_result = Value::Array(vec![
//             Value::scalar(2f64),
//             Value::scalar(1f64),
//             Value::scalar(3f64),
//         ]);
//         assert_eq!(unit!(reverse, input, args), desired_result);
//     }

//     #[test]
//     fn unit_reverse_array_extra_args() {
//         let input = &Value::Array(vec![
//             Value::scalar(3f64),
//             Value::scalar(1f64),
//             Value::scalar(2f64),
//         ]);
//         let args = &[Value::scalar(0f64)];
//         failed!(reverse, input, args);
//     }

//     #[test]
//     fn unit_reverse_ground_control_major_tom() {
//         // Second example from https://shopify.github.io/liquid/filters/reverse/
//         let input = &Value::Array(vec![
//             tos!("G"),
//             tos!("r"),
//             tos!("o"),
//             tos!("u"),
//             tos!("n"),
//             tos!("d"),
//             tos!(" "),
//             tos!("c"),
//             tos!("o"),
//             tos!("n"),
//             tos!("t"),
//             tos!("r"),
//             tos!("o"),
//             tos!("l"),
//             tos!(" "),
//             tos!("t"),
//             tos!("o"),
//             tos!(" "),
//             tos!("M"),
//             tos!("a"),
//             tos!("j"),
//             tos!("o"),
//             tos!("r"),
//             tos!(" "),
//             tos!("T"),
//             tos!("o"),
//             tos!("m"),
//             tos!("."),
//         ]);
//         let args = &[];
//         let desired_result = Value::Array(vec![
//             tos!("."),
//             tos!("m"),
//             tos!("o"),
//             tos!("T"),
//             tos!(" "),
//             tos!("r"),
//             tos!("o"),
//             tos!("j"),
//             tos!("a"),
//             tos!("M"),
//             tos!(" "),
//             tos!("o"),
//             tos!("t"),
//             tos!(" "),
//             tos!("l"),
//             tos!("o"),
//             tos!("r"),
//             tos!("t"),
//             tos!("n"),
//             tos!("o"),
//             tos!("c"),
//             tos!(" "),
//             tos!("d"),
//             tos!("n"),
//             tos!("u"),
//             tos!("o"),
//             tos!("r"),
//             tos!("G"),
//         ]);
//         assert_eq!(unit!(reverse, input, args), desired_result);
//     }

//     #[test]
//     fn unit_reverse_string() {
//         let input = &tos!("abc");
//         let args = &[];
//         failed!(reverse, input, args);
//     }

//     #[test]
//     fn unit_rstrip() {
//         let input = &tos!("test 	 \n \r ");
//         let args = &[];
//         let desired_result = tos!("test");
//         assert_eq!(unit!(rstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_rstrip_leading_sequence() {
//         let input = &tos!(" 	 \n \r test 	 \n \r ");
//         let args = &[];
//         let desired_result = tos!(" 	 \n \r test");
//         assert_eq!(unit!(rstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_rstrip_leading_sequence_only() {
//         let input = &tos!(" 	 \n \r test");
//         let args = &[];
//         let desired_result = tos!(" 	 \n \r test");
//         assert_eq!(unit!(rstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_rstrip_non_string() {
//         let input = &Value::scalar(0f64);
//         let args = &[];
//         let desired_result = tos!("0");
//         assert_eq!(unit!(rstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_rstrip_one_argument() {
//         let input = &tos!(" 	 \n \r test");
//         let args = &[Value::scalar(0f64)];
//         failed!(rstrip, input, args);
//     }

//     #[test]
//     fn unit_rstrip_shopify_liquid() {
//         // One test from https://shopify.github.io/liquid/filters/rstrip/
//         let input = &tos!("          So much room for activities!          ");
//         let args = &[];
//         let desired_result = tos!("          So much room for activities!");
//         assert_eq!(unit!(rstrip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_round() {
//         assert_eq!(
//             unit!(round, Value::scalar(1.1f64), &[]),
//             Value::scalar(1i32)
//         );
//         assert_eq!(
//             unit!(round, Value::scalar(1.5f64), &[]),
//             Value::scalar(2i32)
//         );
//         assert_eq!(unit!(round, Value::scalar(2f64), &[]), Value::scalar(2i32));
//         assert!(round(&Value::scalar(true), &[]).is_err());
//     }

//     #[test]
//     fn unit_round_precision() {
//         assert_eq!(
//             unit!(round, Value::scalar(1.1f64), &[Value::scalar(0i32)]),
//             Value::scalar(1f64)
//         );
//         assert_eq!(
//             unit!(round, Value::scalar(1.5f64), &[Value::scalar(1i32)]),
//             Value::scalar(1.5f64)
//         );
//         assert_eq!(
//             unit!(round, Value::scalar(3.14159f64), &[Value::scalar(3i32)]),
//             Value::scalar(3.142f64)
//         );
//     }

//     #[test]
//     fn unit_size() {
//         assert_eq!(unit!(size, tos!("abc")), Value::scalar(3f64));
//         assert_eq!(
//             unit!(size, tos!("this has 22 characters")),
//             Value::scalar(22f64)
//         );
//         assert_eq!(
//             unit!(
//                 size,
//                 Value::Array(vec![
//                     Value::scalar(0f64),
//                     Value::scalar(1f64),
//                     Value::scalar(2f64),
//                     Value::scalar(3f64),
//                     Value::scalar(4f64),
//                 ])
//             ),
//             Value::scalar(5f64)
//         );
//     }

//     #[test]
//     fn unit_split() {
//         assert_eq!(
//             unit!(split, tos!("a, b, c"), &[tos!(", ")]),
//             Value::Array(vec![tos!("a"), tos!("b"), tos!("c")])
//         );
//         assert_eq!(
//             unit!(split, tos!("a~b"), &[tos!("~")]),
//             Value::Array(vec![tos!("a"), tos!("b")])
//         );
//     }

//     #[test]
//     fn unit_split_bad_split_string() {
//         let input = tos!("a,b,c");
//         let args = &[Value::scalar(1f64)];
//         let desired_result = Value::Array(vec![tos!("a,b,c")]);
//         assert_eq!(unit!(split, input, args), desired_result);
//     }

//     #[test]
//     fn unit_split_no_args() {
//         let input = tos!("a,b,c");
//         let args = &[];
//         let result = split(&input, args);
//         assert!(result.is_err());
//     }

//     #[test]
//     fn unit_strip() {
//         let input = &tos!(" 	 \n \r test 	 \n \r ");
//         let args = &[];
//         let desired_result = tos!("test");
//         assert_eq!(unit!(strip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_leading_sequence_only() {
//         let input = &tos!(" 	 \n \r test");
//         let args = &[];
//         let desired_result = tos!("test");
//         assert_eq!(unit!(strip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_non_string() {
//         let input = &Value::scalar(0f64);
//         let args = &[];
//         let desired_result = tos!("0");
//         assert_eq!(unit!(strip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_one_argument() {
//         let input = &tos!(" 	 \n \r test 	 \n \r ");
//         let args = &[Value::scalar(0f64)];
//         failed!(strip, input, args);
//     }

//     #[test]
//     fn unit_strip_shopify_liquid() {
//         // One test from https://shopify.github.io/liquid/filters/strip/
//         let input = &tos!("          So much room for activities!          ");
//         let args = &[];
//         let desired_result = tos!("So much room for activities!");
//         assert_eq!(unit!(strip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_trailing_sequence_only() {
//         let input = &tos!("test 	 \n \r ");
//         let args = &[];
//         let desired_result = tos!("test");
//         assert_eq!(unit!(strip, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_newlines() {
//         let input = &tos!("a\nb\n");
//         let args = &[];
//         let desired_result = tos!("ab");
//         assert_eq!(unit!(strip_newlines, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_newlines_between_only() {
//         let input = &tos!("a\nb");
//         let args = &[];
//         let desired_result = tos!("ab");
//         assert_eq!(unit!(strip_newlines, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_newlines_leading_only() {
//         let input = &tos!("\nab");
//         let args = &[];
//         let desired_result = tos!("ab");
//         assert_eq!(unit!(strip_newlines, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_newlines_non_string() {
//         let input = &Value::scalar(0f64);
//         let args = &[];
//         let desired_result = tos!("0");
//         assert_eq!(unit!(strip_newlines, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_newlines_one_argument() {
//         let input = &tos!("ab\n");
//         let args = &[Value::scalar(0f64)];
//         failed!(strip_newlines, input, args);
//     }

//     #[test]
//     fn unit_strip_newlines_shopify_liquid() {
//         // Test from https://shopify.github.io/liquid/filters/strip_newlines/
//         let input = &tos!("\nHello\nthere\n");
//         let args = &[];
//         let desired_result = tos!("Hellothere");
//         assert_eq!(unit!(strip_newlines, input, args), desired_result);
//     }

//     #[test]
//     fn unit_strip_newlines_trailing_only() {
//         let input = &tos!("ab\n");
//         let args = &[];
//         let desired_result = tos!("ab");
//         assert_eq!(unit!(strip_newlines, input, args), desired_result);
//     }

//     #[test]
//     fn unit_truncate() {
//         let input = &tos!("I often quote myself.  It adds spice to my conversation.");
//         let args = &[Value::scalar(17i32)];
//         let desired_result = tos!("I often quote ...");
//         assert_eq!(unit!(truncate, input, args), desired_result);
//     }

//     #[test]
//     fn unit_truncate_negative_length() {
//         let input = &tos!("I often quote myself.  It adds spice to my conversation.");
//         let args = &[Value::scalar(-17i32)];
//         let desired_result = tos!("I often quote myself.  It adds spice to my conversation.");
//         assert_eq!(unit!(truncate, input, args), desired_result);
//     }

//     #[test]
//     fn unit_truncate_non_string() {
//         let input = &Value::scalar(10000000f64);
//         let args = &[Value::scalar(5i32)];
//         let desired_result = tos!("10...");
//         assert_eq!(unit!(truncate, input, args), desired_result);
//     }

//     #[test]
//     fn unit_truncate_shopify_liquid() {
//         // Tests from https://shopify.github.io/liquid/filters/truncate/
//         let input = &tos!("Ground control to Major Tom.");
//         let args = &[Value::scalar(20i32)];
//         let desired_result = tos!("Ground control to...");
//         assert_eq!(unit!(truncate, input, args), desired_result);

//         let args = &[Value::scalar(25i32), tos!(", and so on")];
//         let desired_result = tos!("Ground control, and so on");
//         assert_eq!(unit!(truncate, input, args), desired_result);

//         let args = &[Value::scalar(20i32), tos!("")];
//         let desired_result = tos!("Ground control to Ma");
//         assert_eq!(unit!(truncate, input, args), desired_result);
//     }

//     #[test]
//     fn unit_truncate_three_arguments() {
//         let input = &tos!("I often quote myself.  It adds spice to my conversation.");
//         let args = &[Value::scalar(17i32), tos!("..."), Value::scalar(0i32)];
//         failed!(truncate, input, args);
//     }

//     #[test]
//     fn unit_truncate_unicode_codepoints_examples() {
//         // The examples below came from the unicode_segmentation documentation.
//         //
//         // https://unicode-rs.github.io/unicode-segmentation/unicode_segmentation/ ...
//         //               ...  trait.UnicodeSegmentation.html#tymethod.graphemes
//         //
//         // Note that the accents applied to each letter are treated as part of the single grapheme
//         // cluster for the applicable letter.
//         let input = &tos!("Here is an a\u{310}, e\u{301}, and o\u{308}\u{332}.");
//         let args = &[Value::scalar(20i32)];
//         let desired_result = tos!("Here is an a\u{310}, e\u{301}, ...");
//         assert_eq!(unit!(truncate, input, args), desired_result);

//         // Note that the 🇷🇺🇸🇹 is treated as a single grapheme cluster.
//         let input = &tos!("Here is a RUST: 🇷🇺🇸🇹.");
//         let args = &[Value::scalar(20i32)];
//         let desired_result = tos!("Here is a RUST: 🇷🇺...");
//         assert_eq!(unit!(truncate, input, args), desired_result);
//     }

//     #[test]
//     fn unit_truncate_zero_arguments() {
//         let input = &tos!("I often quote myself.  It adds spice to my conversation.");
//         let args = &[];
//         let desired_result = tos!("I often quote myself.  It adds spice to my conv...");
//         assert_eq!(unit!(truncate, input, args), desired_result);
//     }

//     #[test]
//     fn unit_truncatewords_negative_length() {
//         assert_eq!(
//             unit!(
//                 truncatewords,
//                 tos!("one two three"),
//                 &[Value::scalar(-1_i32)]
//             ),
//             tos!("one two three")
//         );
//     }

//     #[test]
//     fn unit_truncatewords_zero_length() {
//         assert_eq!(
//             unit!(
//                 truncatewords,
//                 tos!("one two three"),
//                 &[Value::scalar(0_i32)]
//             ),
//             tos!("...")
//         );
//     }

//     #[test]
//     fn unit_truncatewords_no_truncation() {
//         assert_eq!(
//             unit!(
//                 truncatewords,
//                 tos!("one two three"),
//                 &[Value::scalar(4_i32)]
//             ),
//             tos!("one two three")
//         );
//     }

//     #[test]
//     fn unit_truncatewords_truncate() {
//         assert_eq!(
//             unit!(
//                 truncatewords,
//                 tos!("one two three"),
//                 &[Value::scalar(2_i32)]
//             ),
//             tos!("one two...")
//         );
//         assert_eq!(
//             unit!(
//                 truncatewords,
//                 tos!("one two three"),
//                 &[Value::scalar(2_i32), Value::scalar(1_i32)]
//             ),
//             tos!("one two1")
//         );
//     }

//     #[test]
//     fn unit_truncatewords_empty_string() {
//         assert_eq!(
//             unit!(truncatewords, tos!(""), &[Value::scalar(1_i32)]),
//             tos!("")
//         );
//     }

//     #[test]
//     fn unit_uniq() {
//         let input = &Value::Array(vec![tos!("a"), tos!("b"), tos!("a")]);
//         let args = &[];
//         let desired_result = Value::Array(vec![tos!("a"), tos!("b")]);
//         assert_eq!(unit!(uniq, input, args), desired_result);
//     }

//     #[test]
//     fn unit_uniq_non_array() {
//         let input = &Value::scalar(0f64);
//         let args = &[];
//         failed!(uniq, input, args);
//     }

//     #[test]
//     fn unit_uniq_one_argument() {
//         let input = &Value::Array(vec![tos!("a"), tos!("b"), tos!("a")]);
//         let args = &[Value::scalar(0f64)];
//         failed!(uniq, input, args);
//     }

//     #[test]
//     fn unit_uniq_shopify_liquid() {
//         // Test from https://shopify.github.io/liquid/filters/uniq/
//         let input = &Value::Array(vec![
//             tos!("ants"),
//             tos!("bugs"),
//             tos!("bees"),
//             tos!("bugs"),
//             tos!("ants"),
//         ]);
//         let args = &[];
//         let desired_result = Value::Array(vec![tos!("ants"), tos!("bugs"), tos!("bees")]);
//         assert_eq!(unit!(uniq, input, args), desired_result);
//     }

//     #[test]
//     fn unit_upcase() {
//         assert_eq!(unit!(upcase, tos!("abc")), tos!("ABC"));
//         assert_eq!(
//             unit!(upcase, tos!("Hello World 21")),
//             tos!("HELLO WORLD 21")
//         );
//     }

//     #[test]
//     fn unit_default() {
//         assert_eq!(unit!(default, tos!(""), &[tos!("bar")]), tos!("bar"));
//         assert_eq!(unit!(default, tos!("foo"), &[tos!("bar")]), tos!("foo"));
//         assert_eq!(
//             unit!(default, Value::scalar(0_f64), &[tos!("bar")]),
//             Value::scalar(0_f64)
//         );
//         assert_eq!(
//             unit!(default, Value::Array(vec![]), &[Value::scalar(1_f64)]),
//             Value::scalar(1_f64)
//         );
//         assert_eq!(
//             unit!(
//                 default,
//                 Value::Array(vec![tos!("")]),
//                 &[Value::scalar(1_f64)]
//             ),
//             Value::Array(vec![tos!("")])
//         );
//         assert_eq!(
//             unit!(
//                 default,
//                 Value::Object(Object::new()),
//                 &[Value::scalar(1_f64)]
//             ),
//             Value::scalar(1_f64)
//         );
//         assert_eq!(
//             unit!(default, Value::scalar(false), &[Value::scalar(1_f64)]),
//             Value::scalar(1_f64)
//         );
//         assert_eq!(
//             unit!(default, Value::scalar(true), &[Value::scalar(1_f64)]),
//             Value::scalar(true)
//         );
//     }
// }
