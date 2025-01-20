crate::define_internal_fn!(
    //
    // join all strings in a list
    //

    "join",

    (
        list: RequiredArg<DetachedListType<StringType>> = Arg::method_self(),
        glue: RequiredArg<StringType> = Arg::positional("glue")
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|_, Args { list, glue }, _, _| Ok(Some(RuntimeValue::String(list.join(&glue)))))
}
