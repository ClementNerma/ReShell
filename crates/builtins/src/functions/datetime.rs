use jiff::Zoned;
use reshell_runtime::gc::GcReadOnlyCell;

use crate::{define_internal_fn, types::DateTimeValue};

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
