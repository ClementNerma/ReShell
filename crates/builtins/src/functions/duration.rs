use std::{sync::Arc, time::Duration};

use crate::{define_internal_fn, types::DurationValue};

define_internal_fn!(
    "duration",

    (
        secs: RequiredArg<ExactIntType<u64>> = Arg::positional("secs")
    )

    -> CustomType<DurationValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { secs }, _, _| {
        Ok(Some(RuntimeValue::Custom(Arc::new(DurationValue(
            Duration::from_secs(secs),
        )))))
    })
}
