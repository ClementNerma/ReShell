//!
//! Errors module
//!

use std::{borrow::Cow, num::NonZero};

use parsy::ParsingError;
use reshell_checker::CheckerError;
use reshell_parser::ast::RuntimeCodeRange;

use crate::context::CallStack;

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
    pub infos: Vec<(ExecInfoType, String)>,
}

impl ExecError {
    /// Add an information to the error
    pub fn with_info(
        mut self: Box<Self>,
        info_type: ExecInfoType,
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
    /// Checking of a program failed
    CheckingErr(CheckerError),
    /// A command could not be started
    CommandFailedToStart { message: String },
    /// A command failed
    CommandFailed {
        message: String,
        exit_status: Option<i32>,
    },
    /// A value was thrown and stayed uncaught
    Thrown {
        at: RuntimeCodeRange,
        message: String,
    },
    /// Failure exit (with error code)
    FailureExit { code: NonZero<u8> },
    /// Interrupted by a Ctrl+C press
    CtrlC,
    /// Any other error, represented by a custom message
    Custom(Cow<'static, str>),
    /// Not an actual error (see the enum's docs)
    NotAnError(ExecNotActualError),
    /// Not an actual error, internal use only
    /// Should never appear in a user-facing error
    InternalPropagation(ExecInternalPropagation),
}

/// These is technically no errors so they shouldn't be here ideally,
/// but it would be very hard to propagate them along a very long chain of
/// nested functions up to the top one without wrapping them inside an error structure
#[derive(Debug)]
pub enum ExecNotActualError {
    /// Successfull exit (no error)
    SuccessfulExit,
}

/// These is technically no errors so they shouldn't be here ideally,
/// but it would be very hard to propagate them along a very long chain of
/// nested functions up to the top one without wrapping them inside an error structure
#[derive(Debug)]
pub enum ExecInternalPropagation {
    /// Loop continuation
    LoopContinuation,
    /// Loop breakage
    LoopBreakage,
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
pub enum ExecInfoType {
    Note,
    Tip,
}
