crate::define_internal_fn!(
    "month",

    (
        moment: RequiredArg<DateTimeType> = Arg::method_self()
    )

    -> IntType
);

fn run() -> Runner {
    Runner::new(|_, Args { moment }, _, _| Ok(Some(RuntimeValue::Int(moment.month().into()))))
}
