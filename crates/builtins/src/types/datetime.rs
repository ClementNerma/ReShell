use std::{cmp::Ordering, ops::Deref};

use colored::Color;
use jiff::{Zoned, fmt::rfc2822};
use reshell_prettify::{PrettyPrintable, PrettyPrintablePiece, pretty_printable_string};
use reshell_runtime::values::CustomValueType;

use crate::utils::downcast_custom_value;

/// Date and time value
///
/// Backed by a [`Zoned`]
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
