crate::define_internal_fn!(
    "isEmpty",

    (
        value: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> Some(BoolType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| {
        Ok(Some(RuntimeValue::Bool(
            value.read_promise_no_write().is_empty(),
        )))
    })
}
