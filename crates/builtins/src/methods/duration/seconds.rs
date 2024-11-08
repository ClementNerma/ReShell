use crate::functions::DurationValue;

crate::define_internal_fn!(
    "seconds",

    (
        duration: RequiredArg<CustomType<DurationValue>> = Arg::method_self()
    )

    -> Some(ExactIntType::<u64>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { duration }, _, _| {
        Ok(Some(RuntimeValue::Int(
            duration.as_secs().try_into().unwrap(),
        )))
    })
}
