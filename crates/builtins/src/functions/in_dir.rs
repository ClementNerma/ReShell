use super::cd::change_current_dir;
use crate::{declare_typed_fn_handler, utils::call_fn_checked};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "inDir",

    (
        path: RequiredArg<StringType> = Arg::positional("path"),
        func: RequiredArg<RunFn> = Arg::positional("func")
    )

    -> AnyType
);

declare_typed_fn_handler!(RunFn() -> AnyType);

fn run() -> Runner {
    Runner::new(|at, Args { path, func }, args_at, ctx| {
        let cur_dir = std::env::current_dir().map_err(|err| {
            ctx.throw(
                args_at.path,
                format!("Failed to get current directory: {err}"),
            )
        })?;

        change_current_dir(path, at, ctx)?;

        let result = call_fn_checked(
            &LocatedValue::new(args_at.func, RuntimeValue::Function(func)),
            &RunFn::signature(),
            vec![],
            ctx,
        );

        change_current_dir(cur_dir, at, ctx)?;

        Ok(Some(match result? {
            Some(ret_val) => ret_val.value,
            None => RuntimeValue::Void,
        }))
    })
}
