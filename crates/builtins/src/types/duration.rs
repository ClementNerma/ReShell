use std::{ops::Deref, time::Duration};

use colored::Color;
use reshell_runtime::values::CustomValueType;
use reshell_shared::pretty::{pretty_printable_string, PrettyPrintable, PrettyPrintablePiece};

/// Time duration
///
/// Backed by an STD [`Duration`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DurationValue(pub Duration);

impl Deref for DurationValue {
    type Target = Duration;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
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
    type Context = ();

    fn generate_pretty_data(&self, _: &()) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("duration(", Color::Magenta),
            pretty_printable_string(&format!("{:?}", self.0)),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
