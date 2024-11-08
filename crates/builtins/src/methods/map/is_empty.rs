crate::define_internal_fn!(
    "isEmpty",

    (
        value: RequiredArg<UntypedMapType> = Arg::method_self()
    )

    -> Some(BoolType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| {
        Ok(Some(RuntimeValue::Bool(
            value.read_promise_no_write().is_empty(),
        )))
    })
}
