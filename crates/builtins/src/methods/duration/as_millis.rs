crate::define_internal_fn!(
    "asMillis",

    (
        duration: RequiredArg<DurationType> = Arg::method_self()
    )

    -> ExactIntType<u64>
);

fn run() -> Runner {
    Runner::new(|_, Args { duration }, _, _| {
        Ok(Some(RuntimeValue::Int(
            duration.as_millis().try_into().unwrap(),
        )))
    })
}
