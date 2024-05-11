use reshell_runtime::cmd::CmdSingleArgResult;

use crate::define_internal_fn;

define_internal_fn!(
    "append",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        append: RequiredArg<DetachedListType<CmdArgType>> = Arg::rest("append")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { list, append }, at, ctx| {
        let append = append
            .into_iter()
            .map(|value| match &*value {
                CmdSingleArgResult::Basic(loc_val) => Ok(loc_val.value.clone()),
                CmdSingleArgResult::Flag { name, value: _ } => {
                    Err(ctx.error(name.at(), "Cannot append a flag to a list"))
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        list.write(at.list, ctx)?.extend(append);

        Ok(None)
    })
}
