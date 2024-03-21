use parsy::{CodeRange, FileId, ParsingError};

use crate::{context::ScopeRange, files_map::SourceFile};

pub type ExecResult<T> = Result<T, ExecError>;

#[derive(Debug)]
pub struct ExecError {
    pub at: CodeRange,
    pub in_file: Option<FileId>,
    pub source_file: Option<SourceFile>,
    pub content: ExecErrorContent,
    pub call_stack: CallStack,
    pub scope_range: ScopeRange,
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

#[derive(Debug, Clone)]
pub struct CallStackEntry {
    pub fn_called_at: CodeRange,
    pub previous_scope: u64,
    // pub args: Vec<RuntimeValue>,
}

#[derive(Debug, Clone)]
pub struct CallStack {
    pub history: Vec<CallStackEntry>,
}
