use std::io::Write;

use liquid_error::Result;

use compiler::LiquidOptions;
use compiler::TagBlock;
use compiler::TagTokenIter;
use interpreter::Context;
use interpreter::Renderable;

#[derive(Copy, Clone, Debug)]
struct Comment;

impl Renderable for Comment {
    fn render_to(&self, _writer: &mut Write, _context: &mut Context) -> Result<()> {
        Ok(())
    }
}

pub fn comment_block(
    _tag_name: &str,
    _arguments: TagTokenIter,
    _tokens: &mut TagBlock,
    _options: &LiquidOptions,
) -> Result<Box<Renderable>> {
    Ok(Box::new(Comment))
}

#[cfg(test)]
mod test {
    // use super::*;
    // use compiler;
    // use interpreter;

    // fn options() -> LiquidOptions {
    //     let mut options = LiquidOptions::default();
    //     options
    //         .blocks
    //         .insert("comment", (comment_block as compiler::FnParseBlock).into());
    //     options
    // }

    // fn unit_parse(text: &str) -> String {
    //     let options = options();
    //     let template = compiler::parse(text, &options)
    //         .map(interpreter::Template::new)
    //         .unwrap();

    //     let mut context = Context::new();

    //     template.render(&mut context).unwrap()
    // }

    // #[test]
    // fn test_comment() {
    //     let output = unit_parse("{% comment %} This is a test {% endcomment %}");
    //     assert_eq!(output, "");
    // }
}
