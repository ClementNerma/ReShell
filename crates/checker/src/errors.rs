use parsy::InputRange;

pub type CheckerResult<T = ()> = Result<T, CheckerError>;

/// Error happened in the checker
#[derive(Debug)]
pub struct CheckerError {
    /// Location of the error in the source code
    pub at: InputRange,

    /// Content of the error
    pub msg: String,

    /// Optional additional details
    pub details: Vec<String>,
}

impl CheckerError {
    /// Create a new checker error
    pub fn new(at: InputRange, msg: impl Into<String>) -> Self {
        Self {
            at,
            msg: msg.into(),
            details: vec![],
        }
    }

    /// Add details
    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.details.push(detail.into());
        self
    }
}
