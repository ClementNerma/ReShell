use crate::functions::DurationValue;

crate::define_internal_fn!(
    "subsecMillis",

    (
        duration: RequiredArg<CustomType<DurationValue>> = Arg::method_self()
    )

    -> Some(ExactIntType::<u64>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { duration }, _, _| {
        Ok(Some(RuntimeValue::Int(
            duration.inner.subsec_millis().try_into().unwrap(),
        )))
    })
}
