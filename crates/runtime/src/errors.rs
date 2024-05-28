use parsy::{FileId, ParsingError};
use reshell_parser::ast::RuntimeCodeRange;

use crate::{context::CallStack, files_map::SourceFile};

pub type ExecResult<T> = Result<T, Box<ExecError>>;

#[derive(Debug)]
pub struct ExecError {
    pub has_exit_code: Option<u8>,
    pub at: RuntimeCodeRange,
    pub in_file: Option<FileId>,
    pub source_file: Option<SourceFile>,
    pub content: ExecErrorContent,
    pub call_stack: CallStack,
    pub scope_range: RuntimeCodeRange,
    pub note: Option<String>,
}

impl ExecError {
    pub fn with_note(mut self: Box<Self>, note: impl Into<String>) -> Box<Self> {
        // TODO: allow multiple notes
        assert!(self.note.is_none());
        // =======

        self.note = Some(note.into());
        self
    }
}

#[derive(Debug)]
pub enum ExecErrorContent {
    Str(&'static str),
    String(String),
    ParsingErr(ParsingError),
    CommandFailed {
        message: String,
        exit_status: Option<i32>,
    },
}

impl From<&'static str> for ExecErrorContent {
    fn from(value: &'static str) -> Self {
        Self::Str(value)
    }
}

impl From<String> for ExecErrorContent {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<ParsingError> for ExecErrorContent {
    fn from(value: ParsingError) -> Self {
        Self::ParsingErr(value)
    }
}
