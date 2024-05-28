crate::define_internal_fn!(
    //
    // Create a range value
    //

    "range",

    (
        from: RequiredArg<IntType> = Arg::positional("from"),
        to: RequiredArg<IntType> = Arg::positional("to")
    )

    -> Some(RangeType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { from, to }, _, _| Ok(Some(RuntimeValue::Range { from, to })))
}
