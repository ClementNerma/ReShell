crate::define_internal_fn!(
    "isEmpty",

    (
        value: RequiredArg<StringType> = Arg::method_self()
    )

    -> BoolType
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| Ok(Some(RuntimeValue::Bool(value.is_empty()))))
}
