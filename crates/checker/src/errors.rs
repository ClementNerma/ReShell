use parsy::CodeRange;

pub type CheckerResult<T = ()> = Result<T, CheckerError>;

/// Error happened in the checker
#[derive(Debug)]
pub struct CheckerError {
    /// Location of the error in the source code
    pub at: CodeRange,

    /// Content of the error
    pub msg: String,
}

impl CheckerError {
    /// Create a new checker error
    pub fn new(at: CodeRange, msg: impl Into<String>) -> Self {
        Self {
            at,
            msg: msg.into(),
        }
    }
}
