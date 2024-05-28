use std::time::Instant;

use colored::Color;
use reshell_runtime::{gc::GcReadOnlyCell, values::CustomValueType};
use reshell_shared::pretty::{PrettyPrintable, PrettyPrintablePiece};

crate::define_internal_fn!(
    "instant",

    ()

    -> Some(CustomType::<InstantValue>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args {}, _, _| {
        let value = InstantValue {
            inner: Instant::now(),
        };

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            value,
        )))))
    })
}

/// Time instant
///
/// Backed by an STD [`Instant`]
#[derive(Debug, Clone)]
pub struct InstantValue {
    pub inner: Instant,
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
