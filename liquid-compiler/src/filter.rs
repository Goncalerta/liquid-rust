use std::fmt::{Debug, Display};

use super::FilterArguments;
use liquid_error::Result;
use liquid_interpreter::Context;
use liquid_value::Value;

pub trait FilterParametersReflection {
    fn positional_parameters() -> &'static [ParameterReflection];
    fn keyword_parameters() -> &'static [ParameterReflection];
}

pub trait FilterParameters<'a>: Sized + FilterParametersReflection + Debug + Display {
    type EvaluatedFilterParameters;
    fn from_args(args: FilterArguments) -> Result<Self>;
    fn evaluate(&'a self, context: &'a Context) -> Result<Self::EvaluatedFilterParameters>;
}

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

pub trait Filter: Send + Sync + Debug + Display {
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

impl<T> From<T> for Box<ParseFilter>
where
    T: 'static + ParseFilter,
{
    fn from(filter: T) -> Self {
        Box::new(filter)
    }
}
