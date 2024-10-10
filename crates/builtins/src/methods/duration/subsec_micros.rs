use crate::functions::DurationValue;

crate::define_internal_fn!(
    "subsecMicros",

    (
        duration: RequiredArg<CustomType<DurationValue>> = Arg::method_self()
    )

    -> Some(ExactIntType::<u64>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { duration }, _, _| {
        Ok(Some(RuntimeValue::Int(duration.subsec_micros().into())))
    })
}
