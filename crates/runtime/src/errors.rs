use std::borrow::Cow;

use parsy::{FileId, ParsingError};
use reshell_parser::ast::RuntimeCodeRange;

use crate::{context::CallStack, files_map::SourceFile, values::LocatedValue};

pub type ExecResult<T> = Result<T, Box<ExecError>>;

#[derive(Debug)]
pub struct ExecError {
    pub at: RuntimeCodeRange,
    pub in_file: Option<FileId>,
    pub source_file: Option<SourceFile>,
    pub nature: ExecErrorNature,
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
pub enum ExecErrorNature {
    Custom(Cow<'static, str>),
    ParsingErr(ParsingError),
    CommandFailed {
        message: String,
        exit_status: Option<i32>,
    },
    Exit {
        code: Option<u8>,
    },
    Thrown {
        value: LocatedValue,
    },
}

impl From<&'static str> for ExecErrorNature {
    fn from(value: &'static str) -> Self {
        Self::Custom(Cow::Borrowed(value))
    }
}

impl From<String> for ExecErrorNature {
    fn from(value: String) -> Self {
        Self::Custom(Cow::Owned(value))
    }
}

impl From<ParsingError> for ExecErrorNature {
    fn from(value: ParsingError) -> Self {
        Self::ParsingErr(value)
    }
}
