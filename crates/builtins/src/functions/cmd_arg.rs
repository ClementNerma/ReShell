use reshell_runtime::values::CmdArgValue;

crate::define_internal_fn!(
    "cmdArg",

    (
        value: RequiredArg<AnyType> = Arg::positional("value")
    )

    -> Some(CmdArgType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, args_at, _| {
        Ok(Some(RuntimeValue::CmdArg(Box::new(CmdArgValue::Basic(
            LocatedValue::new(args_at.value, value),
        )))))
    })
}
