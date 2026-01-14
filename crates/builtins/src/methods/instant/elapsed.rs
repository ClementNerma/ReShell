crate::define_internal_fn!(
    "elapsed",

    (
        instant: RequiredArg<InstantType> = Arg::method_self()
    )

    -> DurationType
);

fn run() -> Runner {
    Runner::new(|_, Args { instant }, _, _| Ok(Some(RuntimeValue::Duration(instant.elapsed()))))
}
