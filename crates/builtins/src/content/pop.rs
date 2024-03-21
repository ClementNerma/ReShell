crate::define_internal_fn!(
    //
    // pop a value from a list
    //

    "pop",

    (
        list: RequiredArg<UntypedListType> = Arg::positional("list")
    )

    -> Some(Union2Type::<AnyType, NullType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, ArgsAt { list: list_at }, ctx| {
        Ok(Some(
            list.write(list_at, ctx)?
                .pop()
                .unwrap_or(RuntimeValue::Null),
        ))
    })
}
