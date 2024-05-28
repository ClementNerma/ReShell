crate::define_internal_fn!(
    "isEmpty",

    (
        value: RequiredArg<SpreadType> = Arg::method_self()
    )

    -> Some(BoolType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| Ok(Some(RuntimeValue::Bool(value.is_empty()))))
}
