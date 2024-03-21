use crate::logic::nesting::{handle_nesting, NestingActionType};
use reedline::{ValidationResult, Validator as RlValidator};

pub fn create_validator() -> Box<dyn RlValidator> {
    Box::new(Validator)
}

pub struct Validator;

impl RlValidator for Validator {
    fn validate(&self, line: &str) -> ValidationResult {
        if handle_nesting(line)
            .iter()
            .any(|action| matches!(action.action_type, NestingActionType::Unclosed))
        {
            ValidationResult::Incomplete
        } else {
            ValidationResult::Complete
        }
    }
}
