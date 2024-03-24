use reedline::{ValidationResult, Validator as RlValidator};

use crate::utils::nesting::{detect_nesting_actions, NestingActionType};

pub fn create_validator() -> Box<dyn RlValidator> {
    Box::new(Validator)
}

pub struct Validator;

impl RlValidator for Validator {
    fn validate(&self, line: &str) -> ValidationResult {
        if line.trim_end().ends_with(" \\")
            || detect_nesting_actions(line).iter().any(|action| {
                matches!(
                    action.action_type,
                    NestingActionType::Opening {
                        typ: _,
                        matching_close: false
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
