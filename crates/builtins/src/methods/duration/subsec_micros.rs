use crate::types::DurationValue;

crate::define_internal_fn!(
    "subsecMicros",

    (
        duration: RequiredArg<CustomType<DurationValue>> = Arg::method_self()
    )

    -> ExactIntType<u64>
);

fn run() -> Runner {
    Runner::new(|_, Args { duration }, _, _| {
        Ok(Some(RuntimeValue::Int(duration.subsec_micros().into())))
    })
}
