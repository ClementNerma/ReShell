crate::define_internal_fn!(
    //
    // pop a value from a list
    //

    "pop",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> Some(Union2Type::<AnyType, NullType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, args_at, ctx| {
        Ok(Some(
            list.write(args_at.list, ctx)?
                .pop()
                .unwrap_or(RuntimeValue::Null),
        ))
    })
}
