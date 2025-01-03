crate::define_internal_fn!(
    "isEmpty",

    (
        value: RequiredArg<StringType> = Arg::method_self()
    )

    -> Some(BoolType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| Ok(Some(RuntimeValue::Bool(value.is_empty()))))
}
