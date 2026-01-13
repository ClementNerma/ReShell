use std::time::Duration;

use reshell_runtime::gc::GcReadOnlyCell;

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
        Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
            DurationValue(Duration::from_secs(secs)),
        )))))
    })
}
