crate::define_internal_fn!(
    "last",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> Some(NullableType::<AnyType>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, _, _| {
        Ok(Some(
            list.read_promise_no_write()
                .last()
                .cloned()
                .unwrap_or(RuntimeValue::Null),
        ))
    })
}
