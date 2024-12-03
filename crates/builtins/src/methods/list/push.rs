crate::define_internal_fn!(
    //
    // push a value at the end of a list
    //

    "push",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        value: RequiredArg<AnyType> = Arg::positional("value")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { list, value }, args_at, ctx| {
        list.write(args_at.list, ctx)?.push(value);

        Ok(None)
    })
}
