use std::time::Duration;

use crate::define_internal_fn;

define_internal_fn!(
    "duration",

    (
        secs: RequiredArg<ExactIntType<u64>> = Arg::positional("secs")
    )

    -> DurationType
);

fn run() -> Runner {
    Runner::new(|_, Args { secs }, _, _| {
        Ok(Some(RuntimeValue::Duration(Duration::from_secs(secs))))
    })
}
