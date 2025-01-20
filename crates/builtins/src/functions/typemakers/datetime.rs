use std::ops::Deref;

use colored::Color;
use jiff::{fmt::rfc2822, Zoned};
use reshell_runtime::{
    gc::GcReadOnlyCell, pretty_impl::pretty_printable_string, values::CustomValueType,
};
use reshell_shared::pretty::{PrettyPrintable, PrettyPrintablePiece};

use crate::define_internal_fn;

define_internal_fn!(
    "datetime",

    ()

    -> CustomType<DateTimeValue>
);

fn run() -> Runner {
    Runner::new(|_, Args {}, _, _| {
        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue(Zoned::now()),
        )))))
    })
}

/// Date and time value
///
/// Backed by an [`OffsetDateTime`]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DateTimeValue(Zoned);

impl DateTimeValue {
    pub fn new(datetime: Zoned) -> Self {
        Self(datetime)
    }
}

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
