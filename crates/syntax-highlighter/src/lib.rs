#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

use std::sync::LazyLock;

pub use self::syntax::SyntaxItem;
use self::{elements::ItemType, highlighter::CmdChecker};

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

/// Perform syntax highlighting on a given input
///
/// This function returns a list of syntax items along with their positions in the input.
///
/// The `cmd_checker` argument is a function that determines if a given command (with the provided type)
/// exists or not. This function should run very fast as it will be called for each single command, method
/// and function call in the program.
///
/// Note that the first call to this function make take longer due to the lazy-initialized parser warming up.
/// If you prefer warming up before calling this function, use [`preinit_lazy_syntax_highlighter`].
pub fn syntax_highlight(input: &str, cmd_checker: Option<CmdChecker>) -> Vec<SyntaxItem> {
    self::syntax::compute_highlight_pieces(input, &self::highlighter::RULE_SET, &cmd_checker)
}

/// The syntax highlighter engine is created lazily from a lot of different regular expressions.
///
/// This function triggers the initialization of the engine and ensures calling [`syntax_highlight`] for
/// the first time will not take longer than required.
///
/// Note that this function may take several dozens of milliseconds to complete.
pub fn preinit_lazy_syntax_highlighter() {
    LazyLock::force(&self::highlighter::RULE_SET);
}
