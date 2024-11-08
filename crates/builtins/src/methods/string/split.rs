use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // split a string to produce a list
    //

    "split",

    (
        string: RequiredArg<StringType> = Arg::method_self(),
        separator: RequiredArg<StringType> = Arg::positional("separator")
    )

    -> Some(DetachedListType::<StringType>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string, separator }, _, _| {
        let split = string
            .split(&separator)
            .map(|piece| RuntimeValue::String(piece.to_owned()))
            .collect();

        Ok(Some(RuntimeValue::List(GcCell::new(split))))
    })
}
