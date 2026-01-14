use std::{cmp::Ordering, ops::Deref, time::Duration};

use colored::Color;
use reshell_prettify::{PrettyPrintable, PrettyPrintablePiece, pretty_printable_string};
use reshell_runtime::values::CustomValueType;

use crate::utils::downcast_custom_value;

/// Time duration
///
/// Backed by an STD [`Duration`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
        self.0.eq(&right.0)
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
