use parsy::{CodeRange, ParsingError};

use crate::files_map::SourceFile;

pub type ExecResult<T> = Result<T, ExecError>;

#[derive(Debug)]
pub struct ExecError {
    pub at: CodeRange,
    pub in_fork: bool,
    pub file: SourceFile,
    pub content: ExecErrorContent,
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
