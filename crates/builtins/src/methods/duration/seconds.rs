crate::define_internal_fn!(
    "seconds",

    (
        duration: RequiredArg<DurationType> = Arg::method_self()
    )

    -> ExactIntType<u64>
);

fn run() -> Runner {
    Runner::new(|_, Args { duration }, _, _| {
        Ok(Some(RuntimeValue::Int(
            duration.as_secs().try_into().unwrap(),
        )))
    })
}
