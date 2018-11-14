use std::collections::HashMap;

use super::BoxedBlockParser;
use super::BoxedTagParser;
use super::Include;
use super::NullInclude;

#[derive(Clone)]
pub struct LiquidOptions<'a> {
    pub blocks: HashMap<&'static str, BoxedBlockParser>,
    pub tags: HashMap<&'static str, BoxedTagParser<'a>>,
    pub include_source: Box<Include>,
}

impl<'a> Default for LiquidOptions<'a> {
    fn default() -> LiquidOptions<'a> {
        LiquidOptions {
            blocks: Default::default(),
            tags: Default::default(),
            include_source: Box::new(NullInclude::new()),
        }
    }
}
