use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // split a string to produce a list
    //

    "lines",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> DetachedListType<StringType>
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, _, _| {
        let split = string
            .lines()
            .map(|piece| RuntimeValue::String(piece.to_owned()))
            .collect();

        Ok(Some(RuntimeValue::List(GcCell::new(split))))
    })
}
