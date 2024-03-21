use std::time::Duration;

use colored::Color;
use reshell_runtime::{
    context::Context,
    display::pretty_print_string,
    pretty::{PrettyPrintable, PrettyPrintablePiece},
    values::CustomValueType,
};

#[derive(Debug, Clone)]
pub struct DurationValue {
    pub inner: Duration,
}

impl CustomValueType for DurationValue {
    fn typename(&self) -> &'static str {
        "duration"
    }

    fn typename_static() -> &'static str
    where
        Self: Sized,
    {
        "duration"
    }
}

impl PrettyPrintable for DurationValue {
    fn generate_pretty_data(&self, _: &Context) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("duration(", Color::Magenta),
            pretty_print_string(&format!("{:?}", self.inner)),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
