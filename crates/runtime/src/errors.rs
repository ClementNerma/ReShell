//!
//! Errors module
//!

use std::{borrow::Cow, num::NonZero};

use parsy::ParsingError;
use reshell_checker::CheckerError;
use reshell_parser::ast::RuntimeCodeRange;

use crate::context::CallStack;

/// Result of an action that may have resulted in an execution error
pub type ExecResult<T> = Result<T, ExecError>;

/// An error that occured during execution (runtime)
#[derive(Debug)]
pub enum ExecError {
    /// Actual errors ([`ExecActualError`]) are always wrapped in a [`Box`] to avoid
    /// moving very large [`ExecError`] values around.
    ///
    /// Given an error is either critical or related to a function throwing a value,
    /// the allocation overhead is acceptable here
    ActualError(Box<ExecActualError>),

    /// These are not actually errors, but informations that need to be propagated
    /// through lots of nested function calls, which would be very hard without the
    /// `Try` trait on `Err`
    InternalPropagation(ExecInternalPropagation),

    /// These are not actually errors, but informations that need to be propagated
    /// through lots of nested function calls, which would be very hard without the
    /// `Try` trait on `Err`.
    ///
    /// These can't be handled in any way by the runtime except at the very top, so
    /// they are separated from [`ExecInternalPropagation`] for easier handling.
    TopPropagation(ExecTopPropagation),
}

#[derive(Debug)]
pub struct ExecActualError {
    /// Location where the error happened
    pub at: RuntimeCodeRange,
    /// Nature of the error
    pub nature: ExecActualErrorNature,
    /// Call stack
    pub call_stack: CallStack,
    /// Additional informations
    pub infos: Vec<(ExecInfoType, String)>,
}

impl ExecActualError {
    /// Add an information to the error
    pub fn add_info(&mut self, info_type: ExecInfoType, content: impl Into<String>) {
        self.infos.push((info_type, content.into()));
    }
}

/// Nature of an execution error
#[derive(Debug)]
pub enum ExecActualErrorNature {
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
}

impl From<&'static str> for ExecActualErrorNature {
    fn from(value: &'static str) -> Self {
        Self::Custom(Cow::Borrowed(value))
    }
}

impl From<String> for ExecActualErrorNature {
    fn from(value: String) -> Self {
        Self::Custom(Cow::Owned(value))
    }
}

impl From<ParsingError> for ExecActualErrorNature {
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

/// These are not actually errors, but informations that need to be propagated
/// through lots of nested function calls, which would be very hard without the
/// `Try` trait on `Err`
#[derive(Debug)]
pub enum ExecInternalPropagation {
    /// Loop continuation
    LoopContinuation,

    /// Loop breakage
    LoopBreakage,
}

/// These are not actually errors, but informations that need to be propagated
/// through lots of nested function calls, which would be very hard without the
/// `Try` trait on `Err`.
///
/// These can't be handled in any way by the runtime except at the very top, so
/// they are separated from [`ExecInternalPropagation`] for easier handling.
#[derive(Debug)]
pub enum ExecTopPropagation {
    /// Exit the program successfully
    SuccessfulExit,
}
