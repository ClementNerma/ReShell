use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    "cmdArgs",

    (
        args: RequiredArg<DetachedListType<CmdArgType>> = Arg::rest("args")
    )

    -> Some(DetachedListType::<CmdArgType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { args }, _, _| {
        Ok(Some(RuntimeValue::List(GcCell::new(
            args.into_iter().map(RuntimeValue::CmdArg).collect(),
        ))))
    })
}
