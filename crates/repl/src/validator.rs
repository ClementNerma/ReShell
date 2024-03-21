use crate::logic::nesting::{handle_nesting, NestingCharAction};
use reedline::{ValidationResult, Validator as RlValidator};

pub fn create_validator() -> Box<dyn RlValidator> {
    Box::new(Validator)
}

pub struct Validator;

impl RlValidator for Validator {
    fn validate(&self, line: &str) -> ValidationResult {
        if handle_nesting(line)
            .iter()
            .any(|(_, _, action)| *action == NestingCharAction::Unclosed)
        {
            ValidationResult::Incomplete
        } else {
            ValidationResult::Complete
        }
    }
}
