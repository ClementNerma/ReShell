use reedline::{ValidationResult, Validator as RlValidator};

use crate::utils::nesting::{NestingAction, NestingActionType, detect_nesting_actions};

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
        // Check for line continuation backslashes
        if line.trim_end().ends_with(" \\")
        // And for unclosed nestings
            || detect_nesting_actions(line, false).iter().any(|action| {
                matches!(
                    action,
                    NestingAction {
                        action_type: NestingActionType::Opening {
                            typ: _,
                            matching_close: false
                        },
                        ..
                    }
                )
            })
        {
            ValidationResult::Incomplete
        } else {
            ValidationResult::Complete
        }
    }
}
