use crate::define_internal_fn;

define_internal_fn!(
    "assert",

    (
        bool: RequiredArg<BoolType> = Arg::positional("bool")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|_, Args { bool }, args_at, ctx| {
        if bool {
            Ok(None)
        } else {
            Err(ctx.throw(args_at.bool, "predicate failed"))
        }
    })
}
