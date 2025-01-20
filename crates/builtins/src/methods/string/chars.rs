use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // split a string to produce a list
    //

    "chars",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> DetachedListType<StringType>
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, _, _| {
        Ok(Some(RuntimeValue::List(GcCell::new(
            string
                .chars()
                .map(|c| RuntimeValue::String(c.to_string()))
                .collect(),
        ))))
    })
}
