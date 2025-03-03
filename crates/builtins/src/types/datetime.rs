use std::ops::Deref;

use colored::Color;
use jiff::{fmt::rfc2822, Zoned};
use reshell_runtime::values::CustomValueType;
use reshell_shared::pretty::{pretty_printable_string, PrettyPrintable, PrettyPrintablePiece};

/// Date and time value
///
/// Backed by an [`OffsetDateTime`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DateTimeValue(pub Zoned);

impl Deref for DateTimeValue {
    type Target = Zoned;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl CustomValueType for DateTimeValue {
    fn typename(&self) -> &'static str {
        "datetime"
    }

    fn typename_static() -> &'static str
    where
        Self: Sized,
    {
        "datetime"
    }
}

impl PrettyPrintable for DateTimeValue {
    type Context = ();

    fn generate_pretty_data(&self, _: &()) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("datetime(", Color::Magenta),
            pretty_printable_string(&rfc2822::to_string(&self.0).unwrap()),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
