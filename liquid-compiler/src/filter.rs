use std::fmt::Debug;

use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_value::Value;
use super::FilterArguments;

pub struct ParameterReflection {
    pub name: &'static str,
    pub description: &'static str,
    pub is_optional: bool,
}

pub trait FilterReflection {
    fn name(&self) -> &'static str;
    fn description(&self) -> &'static str;

    fn positional_parameters(&self) -> &'static [ParameterReflection];
    fn keyword_parameters(&self) -> &'static [ParameterReflection];
}

pub trait Filter: Send + Sync + Debug {
    // This will evaluate the expressions and evaluate the filter.
    fn evaluate(&self, input: &Value, context: &Context) -> Result<Value>;
}

/// A trait for creating custom tags. This is a simple type alias for a function.
///
/// This function will be called whenever the parser encounters a tag and returns
/// a new [Renderable](trait.Renderable.html) based on its parameters. The received parameters
/// specify the name of the tag, the argument [Tokens](lexer/enum.Token.html) passed to

/// the tag and the global [`Language`](struct.Language.html).
pub trait ParseFilter: Send + Sync + ParseFilterClone + FilterReflection {
    /// Filter `input` based on `arguments`.
    fn parse(&self, arguments: FilterArguments) -> Result<Box<Filter>>;
}

// TODO boxed optimization
pub type BoxedFilterParser = Box<ParseFilter>;
impl<T: ParseFilter + 'static> From<Box<T>> for BoxedFilterParser {
    fn from(filter: Box<T>) -> BoxedFilterParser {
        filter
    }
}

/// Support cloning of `Box<ParseFilter>`.
pub trait ParseFilterClone {
    /// Cloning of `dyn ParseFilter`.
    fn clone_box(&self) -> Box<ParseFilter>;
}

impl<T> ParseFilterClone for T
where
    T: 'static + ParseFilter + Clone,
{
    fn clone_box(&self) -> Box<ParseFilter> {
        Box::new(self.clone())
    }
}

impl Clone for Box<ParseFilter> {
    fn clone(&self) -> Box<ParseFilter> {
        self.clone_box()
    }
}

// /// Function signature that can act as a `ParseFilter`.
// pub type FnParseFilter = fn(&Value, FilterArguments) -> Result<Box<Filter>>;

// #[derive(Clone)]
// struct FnFilterParser {
//     filter: FnParseFilter,
// }

// impl FnFilterParser {
//     fn new(filter: FnParseFilter) -> Self {
//         Self { filter }
//     }
// }

// impl ParseFilter for FnFilterParser {
//     fn parse(&self, input: &Value, arguments: FilterArguments) -> Result<Box<Filter>> {
//         (self.filter)(input, arguments)
//     }
// }

// #[derive(Clone)]
// enum EnumValueFilter {
//     Fun(FnFilterParser),
//     Heap(Box<ParseFilter>),
// }

// /// Custom `Box<ParseFilter>` with a `FnParseFilter` optimization.
// #[derive(Clone)]
// pub struct BoxedParseFilter {
//     filter: EnumValueFilter,
// }

// impl ParseFilter for BoxedParseFilter {
//     fn parse(&self, input: &Value, arguments: &[Value]) -> Result<Box<Filter>> {
//         match self.filter {
//             EnumValueFilter::Fun(ref f) => f.parse(input, arguments),
//             EnumValueFilter::Heap(ref f) => f.parse(input, arguments),
//         }
//     }
// }

// impl From<FnParseFilter> for BoxedValueFilter {
//     fn from(filter: FnParseFilter) -> BoxedValueFilter {
//         let filter = EnumValueFilter::Fun(FnFilterParser::new(filter));
//         Self { filter }
//     }
// }

// impl From<Box<ParseFilter>> for BoxedValueFilter {
//     fn from(filter: Box<ParseFilter>) -> BoxedValueFilter {
//         let filter = EnumValueFilter::Heap(filter);
//         Self { filter }
//     }
// }
