use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // split a string to produce a list
    //

    "split",

    (
        string: RequiredArg<StringType> = Arg::positional("string"),
        sep: RequiredArg<StringType> = Arg::positional("sep")
    )

    -> Some(DetachedListType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string, sep }, _, _| {
        let split = string
            .split(&sep)
            .map(|piece| RuntimeValue::String(piece.to_owned()))
            .collect();

        Ok(Some(RuntimeValue::List(GcCell::new(split))))
    })
}
