use parsy::Parser;
use reedline::{ValidationResult, Validator as RlValidator};
use reshell_parser::program;

pub fn create_validator() -> Box<dyn RlValidator> {
    Box::new(Validator)
}

pub struct Validator;

impl RlValidator for Validator {
    fn validate(&self, line: &str) -> ValidationResult {
        let parser = program();

        if parser.parse_str(line).is_err() {
            ValidationResult::Incomplete
        } else {
            ValidationResult::Complete
        }
    }
}
