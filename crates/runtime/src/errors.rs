use std::borrow::Cow;

use parsy::ParsingError;
use reshell_parser::ast::RuntimeCodeRange;

use crate::{context::CallStack, values::LocatedValue};

/// Result of an action that may have resulted in an execution error
pub type ExecResult<T> = Result<T, Box<ExecError>>;

/// An error that occured during execution (runtime)
#[derive(Debug)]
pub struct ExecError {
    /// Location where the error happened
    pub at: RuntimeCodeRange,
    /// Nature of the error
    pub nature: ExecErrorNature,
    /// Call stack
    pub call_stack: CallStack,
    /// Additional informations
    pub infos: Vec<(ExecErrorInfoType, String)>,
}

impl ExecError {
    /// Add an information to the error
    pub fn with_info(
        mut self: Box<Self>,
        info_type: ExecErrorInfoType,
        content: impl Into<String>,
    ) -> Box<Self> {
        self.infos.push((info_type, content.into()));
        self
    }
}

/// Nature of an execution error
#[derive(Debug)]
pub enum ExecErrorNature {
    /// Parsing of a program failed
    ParsingErr(ParsingError),
    /// A command could not be started
    CommandFailedToStart { message: String },
    /// A command failed
    CommandFailed {
        message: String,
        exit_status: Option<i32>,
    },
    /// A value was thrown and stayed uncaught
    Thrown { value: LocatedValue },
    /// Program requested to exit
    Exit { code: Option<u8> },
    /// Interrupted by a Ctrl+C press
    CtrlC,
    /// Any other error, represented by a custom message
    Custom(Cow<'static, str>),
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

/// Error's additional information type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExecErrorInfoType {
    Note,
    Tip,
}
