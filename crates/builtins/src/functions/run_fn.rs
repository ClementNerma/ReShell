use reshell_runtime::{cmd::CmdArgResult, functions::FnPossibleCallArgs};

use crate::utils::{call_fn_checked_with_parsed_args, forge_basic_fn_signature};

crate::define_internal_fn!(
    "runFn",

    (
        func: RequiredArg<TypedFunctionType> = exec_fn_type(),
        args: RequiredArg<SpreadType> = Arg::positional("spread")
    )

    -> None
);

fn exec_fn_type() -> RequiredArg<TypedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("transform_fn"),
        TypedFunctionType::new(forge_basic_fn_signature(vec![], None)),
    )
}

fn run() -> Runner {
    Runner::new(|_, Args { func, args }, args_at, ctx| {
        let loc_val = call_fn_checked_with_parsed_args(
            &LocatedValue::new(RuntimeValue::Function(func), args_at.func),
            exec_fn_type().base_typing().signature(),
            FnPossibleCallArgs::Internal(vec![CmdArgResult::Spreaded(Vec::clone(&args))]),
            ctx,
        )?;

        Ok(loc_val.map(|loc_val| loc_val.value))
    })
}
