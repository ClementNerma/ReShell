crate::define_internal_fn!(
    //
    // join all strings in a list
    //

    "join",

    (
        list: RequiredArg<DetachedListType<StringType>> = Arg::positional("list"),
        glue: RequiredArg<StringType> = Arg::positional("glue")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list, glue }, _, _| Ok(Some(RuntimeValue::String(list.join(&glue)))))
}
