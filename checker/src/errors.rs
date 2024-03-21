use parsy::CodeRange;

pub type CheckerResult<T = ()> = Result<T, CheckerError>;

pub struct CheckerError {
    pub at: CodeRange,
    pub msg: String,
}

impl CheckerError {
    pub fn new(at: CodeRange, msg: impl Into<String>) -> Self {
        Self {
            at,
            msg: msg.into(),
        }
    }
}
