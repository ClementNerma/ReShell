use colored::Color;
use reshell_runtime::{
    display::pretty_print_string,
    gc::GcReadOnlyCell,
    pretty::{PrettyPrintable, PrettyPrintablePiece},
    values::CustomValueType,
};
use time::{
    util::local_offset::{set_soundness, Soundness},
    OffsetDateTime, UtcOffset,
};

use crate::define_internal_fn;

define_internal_fn!(
    "datetime",

    ()

    -> Some(CustomType::<DateTimeValue>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args {}, ArgsAt {}, _| {
        let offset = get_utc_offset();

        let now = OffsetDateTime::now_utc().to_offset(offset);

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DateTimeValue { inner: now },
        )))))
    })
}

/// UNSAFE CODE
///
/// Due to [`std::env::set_var`] being unsound, we can only get the UtcOffset
/// if the program is single-threaded.
///
/// This means that ReShell's runtime is actually only sound if the program
/// is single-threaded.
fn get_utc_offset() -> UtcOffset {
    unsafe {
        set_soundness(Soundness::Unsound);
    }

    let offset = UtcOffset::current_local_offset().unwrap();

    unsafe {
        set_soundness(Soundness::Sound);
    }

    offset
}

#[derive(Debug, Clone)]
pub struct DateTimeValue {
    pub inner: OffsetDateTime,
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
    fn generate_pretty_data(&self, _: &Context) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("datetime(", Color::Magenta),
            pretty_print_string(&self.inner.to_string()),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}