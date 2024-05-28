use reedline::{ValidationResult, Validator as RlValidator};

use crate::utils::nesting::detect_nesting_actions;

pub fn create_validator() -> Box<dyn RlValidator> {
    Box::new(Validator)
}

/// Custom line validator implementation for [`reedline`].
///
/// Check if e.g. a string or parenthesis-wrapped expression is left unclosed, in which case
/// a multi-line editor will be provided.
pub struct Validator;

impl RlValidator for Validator {
    fn validate(&self, line: &str) -> ValidationResult {
        if line.trim_end().ends_with(" \\")
            || detect_nesting_actions(line, false).final_nesting_level > 0
        {
            ValidationResult::Incomplete
        } else {
            ValidationResult::Complete
        }
    }
}
