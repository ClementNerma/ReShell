use std::{collections::HashSet, sync::LazyLock};

use parsy::{FileId, Parser, helpers::get_context, timed::LazilyDefined};

use self::blocks::RAW_BLOCK;
use crate::{ast::Program, files_map::SourceFile, use_basic_parsers};

mod basics;
mod blocks;
mod cmd_calls;
mod exprs;
mod functions;
mod instrs;
mod types;
mod values;

/// The parsing context, required to parse a ReShell program
pub struct ParserContext {
    pub load_file: FileLoader,
}

/// A callback to load files when an 'include' statement is encountered
pub type FileLoader = Box<dyn Fn(String, FileId) -> Result<SourceFile, String> + Send + Sync>;

/// Parser for a ReShell program
///
/// Requires to set up a parser context by providing a [`ParserContext`] to [`parsy::ParserInput::new_with_ctx`]
pub static PROGRAM: LazilyDefined<Program> = LazilyDefined::new(|| {
    use_basic_parsers!(msnl);

    get_context::<ParserContext>()
        .ignore_then(
            RAW_BLOCK
                .static_ref()
                .spanned()
                .padded_by(msnl)
                .full()
                .critical("unexpected symbol")
                .map(|content| Program { content }),
        )
        .erase_type()
});

pub static DELIMITER_CHARS: LazyLock<HashSet<char>> = LazyLock::new(|| {
    HashSet::from([
        '(', ')', '[', ']', '{', '}', '<', '>', ';', '|', '\'', '"', '`', '$', '#', '^',
    ])
});

// Usage: .debug(simple_debug) after any parser
#[allow(dead_code)]
fn simple_debug<T: std::fmt::Debug>(d: parsy::tails::DebugType<'_, '_, T>) {
    println!("{d:#?}");
}
