use reshell_runtime::gc::GcReadOnlyCell;

use crate::functions::{DurationValue, InstantValue};

crate::define_internal_fn!(
    "elapsed",

    (
        instant: RequiredArg<CustomType<InstantValue>> = Arg::method_self()
    )

    -> CustomType<DurationValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { instant }, _, _| {
        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DurationValue::new(instant.elapsed()),
        )))))
    })
}
