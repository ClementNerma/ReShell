use reshell_parser::ast::FnCallNature;
use reshell_runtime::{
    cmd::CmdArgResult,
    functions::{call_fn_value, FnCallInfos, FnPossibleCallArgs},
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

crate::define_internal_fn!(
    "runFn",

    (
        func: RequiredArg<AnyType> = Arg::positional("func"),
        args: RequiredArg<DetachedListType<CmdArgType>> = Arg::rest("args")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { func, args }, args_at, ctx| match func {
        RuntimeValue::Function(func) => {
            let loc_val = call_fn_value(
                args_at.func,
                &func,
                FnCallInfos {
                    args: FnPossibleCallArgs::Internal(
                        args.into_iter()
                            .map(|arg| CmdArgResult::Single((*arg).clone()))
                            .collect(),
                    ),
                    nature: FnCallNature::Variable,
                    piped: None,
                },
                ctx,
            )?;

            Ok(loc_val.map(|loc_val| loc_val.value))
        }

        _ => Err(ctx.error(
            args_at.func,
            format!(
                "expected a function, found a: {}",
                func.compute_type()
                    .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
            ),
        )),
    })
}
