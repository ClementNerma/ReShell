use reshell_runtime::cmd::SingleCmdArgResult;

use crate::define_internal_fn;

define_internal_fn!(
    "prepend",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        prepend: RequiredArg<DetachedListType<CmdArgType>> = Arg::rest("prepend")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { list, prepend }, at, ctx| {
        let prepend = prepend
            .into_iter()
            .map(|value| match &*value {
                SingleCmdArgResult::Basic(loc_val) => Ok(loc_val.value.clone()),
                SingleCmdArgResult::Flag { name, value: _ } => {
                    Err(ctx.error(name.at(), "Cannot prepend a flag to a list"))
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        list.write(at.list, ctx)?.splice(0..0, prepend);

        Ok(None)
    })
}
