use liquid_interpreter::Renderable;

use super::error::Result;
use super::LiquidOptions;
use super::TagToken;

/// A trait for creating custom tags. This is a simple type alias for a function.
///
/// This function will be called whenever the parser encounters a tag and returns
/// a new [Renderable](trait.Renderable.html) based on its parameters. The received parameters
/// specify the name of the tag, the argument [Tokens](lexer/enum.Token.html) passed to
/// the tag and the global [`LiquidOptions`](struct.LiquidOptions.html).
pub trait ParseTag<'a>: Send + Sync + ParseTagClone {
    fn parse(
        &'a self,
        tag_name: &str,
        arguments: Box<Iterator<Item=TagToken<'a>>+'a>,
        options: &LiquidOptions,
    ) -> Result<Box<Renderable>>;
}

pub trait ParseTagClone {
    fn clone_box(&self) -> Box<ParseTag>;
}

impl<'a, T> ParseTagClone for T
where
    T: 'static + ParseTag<'a> + Clone,
{
    fn clone_box(&self) -> Box<ParseTag<'a>+'a> {
        Box::new(self.clone())
    }
}

impl<'a> Clone for Box<ParseTag<'a>+'a> {
    fn clone(&self) -> Box<ParseTag> {
        self.clone_box()
    }
}
pub type FnParseTag<'a> = fn(&str, Box<Iterator<Item=TagToken<'a>>+'a>, &LiquidOptions) -> Result<Box<Renderable>>;


#[derive(Clone)]
struct FnTagParser<'a> {
    parser: FnParseTag<'a>,
}

impl<'a> FnTagParser<'a> {
    fn new(parser: FnParseTag<'a>) -> Self {
        Self { parser }
    }
}

impl<'a> ParseTag<'a> for FnTagParser<'a> {
    fn parse(
        &self,
        tag_name: &str,
        arguments: Box<Iterator<Item=TagToken<'a>>+'a>,
        options: &LiquidOptions,
    ) -> Result<Box<Renderable>> {
        (self.parser)(tag_name, arguments, options)
    }
}

#[derive(Clone)]
enum TagParserEnum<'a> {
    Fun(FnTagParser<'a>),
    Heap(Box<ParseTag<'a> + 'a>),
}

#[derive(Clone)]
pub struct BoxedTagParser<'a> {
    parser: TagParserEnum<'a>,
}

impl<'a> ParseTag<'a> for BoxedTagParser<'a> {
    fn parse(
        &self,
        tag_name: &str,
        arguments: Box<Iterator<Item=TagToken<'a>>+'a>,
        options: &LiquidOptions,
    ) -> Result<Box<Renderable>> {
        match self.parser {
            TagParserEnum::Fun(ref f) => f.parse(tag_name, arguments, options),
            TagParserEnum::Heap(ref f) => f.parse(tag_name, arguments, options),
        }
    }
}

impl<'a> From<FnParseTag<'a>> for BoxedTagParser<'a> {
    fn from(parser: FnParseTag<'a>) -> BoxedTagParser<'a> {
        let parser = TagParserEnum::Fun(FnTagParser::new(parser));
        Self { parser }
    }
}

impl<'a> From<Box<ParseTag<'a>>> for BoxedTagParser<'a> {
    fn from(parser: Box<ParseTag<'a>+'a>) -> BoxedTagParser<'a> {
        let parser = TagParserEnum::Heap(parser);
        Self { parser }
    }
}
