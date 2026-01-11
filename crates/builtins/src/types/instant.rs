use std::{cmp::Ordering, ops::Deref, time::Instant};

use colored::Color;
use reshell_prettify::{PrettyPrintable, PrettyPrintablePiece};
use reshell_runtime::values::CustomValueType;

use crate::utils::downcast_custom_value;

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

    fn supports_ord(&self) -> bool {
        true
    }

    fn ord(&self, right: &dyn CustomValueType) -> Ordering {
        let right = downcast_custom_value::<Self>(right).unwrap();
        self.0.cmp(&right.0)
    }

    fn supports_eq(&self) -> bool {
        true
    }

    fn eq(&self, right: &dyn CustomValueType) -> bool {
        let right = downcast_custom_value::<Self>(right).unwrap();
        self.0 == right.0
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
