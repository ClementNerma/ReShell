use std::sync::Arc;

use crate::types::{DurationValue, InstantValue};

crate::define_internal_fn!(
    "elapsed",

    (
        instant: RequiredArg<CustomType<InstantValue>> = Arg::method_self()
    )

    -> CustomType<DurationValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { instant }, _, _| {
        Ok(Some(RuntimeValue::Custom(Arc::new(DurationValue(
            instant.elapsed(),
        )))))
    })
}
