use reshell_runtime::gc::GcReadOnlyCell;

use crate::functions::{DurationValue, InstantValue};

crate::define_internal_fn!(
    "elapsed",

    (
        instant: RequiredArg<CustomType<InstantValue>> = Arg::method_self()
    )

    -> Some(CustomType::<DurationValue>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { instant }, _, _| {
        let duration = DurationValue {
            inner: instant.inner.elapsed(),
        };

        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            duration,
        )))))
    })
}
