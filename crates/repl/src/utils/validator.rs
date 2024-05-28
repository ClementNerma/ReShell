use reedline::{ValidationResult, Validator as RlValidator};

use super::nesting::{detect_nesting_actions, NestingActionType};

pub fn create_validator() -> Box<dyn RlValidator> {
    Box::new(Validator)
}

pub struct Validator;

impl RlValidator for Validator {
    fn validate(&self, line: &str) -> ValidationResult {
        if detect_nesting_actions(line)
            .iter()
            .any(|action| matches!(action.action_type, NestingActionType::Unclosed(_)))
        {
            ValidationResult::Incomplete
        } else {
            ValidationResult::Complete
        }
    }
}
