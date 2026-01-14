crate::define_internal_fn!(
    "year",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self()
    )

    -> IntType
);

fn run() -> Runner {
    Runner::new(|_, Args { moment }, _, _| Ok(Some(RuntimeValue::Int(moment.year().into()))))
}
