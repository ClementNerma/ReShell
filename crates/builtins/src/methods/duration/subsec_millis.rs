crate::define_internal_fn!(
    "subsecMillis",

    (
        duration: RequiredArg<DurationType> = Arg::method_self()
    )

    -> ExactIntType<u64>
);

fn run() -> Runner {
    Runner::new(|_, Args { duration }, _, _| {
        Ok(Some(RuntimeValue::Int(duration.subsec_millis().into())))
    })
}
