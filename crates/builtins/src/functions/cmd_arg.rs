use reshell_runtime::values::CmdArgValue;

crate::define_internal_fn!(
    "cmdArg",

    (
        value: RequiredArg<AnyType> = Arg::positional("value")
    )

    -> Some(CmdArgType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, args_at, _| {
        Ok(Some(RuntimeValue::CmdArg(Box::new(CmdArgValue::Basic(
            LocatedValue::new(value, args_at.value),
        )))))
    })
}
