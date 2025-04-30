#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

use std::sync::{Arc, Mutex};

use self::elements::ItemType;
pub use self::syntax::HighlightedPiece;

mod coverage;
pub mod elements;
mod highlighter;
pub mod nesting;
mod syntax;

#[derive(Debug, Clone, Copy)]
pub enum CheckCmdType {
    Method,
    Function,
    ExternalCmd,
    BroadCmd, // External cmd or function name or alias
}

pub fn syntax_highlight(
    input: &str,
    cmd_checker: impl FnMut(&str, CheckCmdType) -> bool + Send + Sync + 'static,
) -> Vec<HighlightedPiece> {
    self::syntax::compute_highlight_pieces(
        input,
        &self::highlighter::RULE_SET,
        &Arc::new(Mutex::new(Box::new(cmd_checker))),
    )
}
