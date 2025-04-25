use std::{ops::Deref, time::Instant};

use colored::Color;
use reshell_prettify::{PrettyPrintable, PrettyPrintablePiece};
use reshell_runtime::values::CustomValueType;

/// Time instant
///
/// Backed by an STD [`Instant`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstantValue(pub Instant);

impl Deref for InstantValue {
    type Target = Instant;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl CustomValueType for InstantValue {
    fn typename(&self) -> &'static str {
        "instant"
    }

    fn typename_static() -> &'static str
    where
        Self: Sized,
    {
        "instant"
    }
}

impl PrettyPrintable for InstantValue {
    type Context = ();

    fn generate_pretty_data(&self, _: &()) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("instant(", Color::Magenta),
            PrettyPrintablePiece::colored_atomic("<internal>", Color::BrightBlack),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
