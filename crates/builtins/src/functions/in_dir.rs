use crate::utils::{call_fn_checked, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "inDir",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        func: RequiredArg<TypedFunctionType> = func_type()
    )

    -> None
);

fn func_type() -> RequiredArg<TypedFunctionType> {
    Arg::new(
        ArgNames::Positional("func"),
        TypedFunctionType::new(forge_basic_fn_signature(vec![], None)),
    )
}

fn run() -> Runner {
    Runner::new(|at, Args { path, func }, args_at, ctx| {
        let filter = LocatedValue::new(args_at.func, RuntimeValue::Function(func));

        let cur_dir = std::env::current_dir().map_err(|err| {
            ctx.error(
                args_at.path,
                format!("Failed to get current directory: {err}"),
            )
        })?;

        std::env::set_current_dir(&path).map_err(|err| {
            ctx.error(
                args_at.path,
                format!("Failed to go to directory {path:?}: {err}"),
            )
        })?;

        let result = call_fn_checked(&filter, func_type().base_typing().signature(), vec![], ctx);

        std::env::set_current_dir(&cur_dir).map_err(|err| {
            ctx.error(
                at,
                format!("Failed to go back to previous directory {cur_dir:?}: {err}"),
            )
        })?;

        result.map(|_| None)
    })
}
