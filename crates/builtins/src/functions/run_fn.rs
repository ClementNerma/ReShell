use reshell_parser::ast::FnCallNature;
use reshell_runtime::{
    cmd::{CmdArgResult, SingleCmdArgResult},
    functions::{call_fn_value, FnCallInfos, FnPossibleCallArgs},
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

crate::define_internal_fn!(
    "runFn",

    (
        func: RequiredArg<AnyType> = Arg::positional("func"),
        args: UntypedRestArg = UntypedRestArg::rest("args")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { func, args }, args_at, ctx| match func {
        // TODO: how to differentiate provided positional cmdflag with actual flags that have been converted to cmdflags?
        RuntimeValue::Function(func) => {
            let loc_val = call_fn_value(
                args_at.func,
                &func,
                FnCallInfos {
                    args: FnPossibleCallArgs::Internal(
                        args.into_iter()
                            .map(SingleCmdArgResult::from)
                            .map(CmdArgResult::Single)
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
