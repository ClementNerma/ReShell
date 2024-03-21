use parsy::{CodeRange, FileId, ParsingError};

use crate::{
    context::{CallStack, ScopeRange},
    files_map::SourceFile,
};

pub type ExecResult<T> = Result<T, ExecError>;

#[derive(Debug)]
pub struct ExecError {
    pub has_exit_code: Option<u8>,
    pub at: CodeRange,
    pub in_file: Option<FileId>,
    pub source_file: Option<SourceFile>,
    pub content: ExecErrorContent,
    pub call_stack: CallStack,
    pub scope_range: ScopeRange,
    pub note: Option<String>,
}

impl ExecError {
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
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
