use crate::define_internal_fn;

define_internal_fn!(
    "at",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        index: RequiredArg<ExactIntType<usize>> = Arg::positional("key"),
        or_else: OptionalArg<AnyType> = Arg::long_flag("or-else")
    )

    -> Some(AnyType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             list,
             index,
             or_else,
         },
         args_at,
         ctx| {
            let items = list.read_promise_no_write();

            match items.get(index) {
                Some(value) => Ok(Some(value.clone())),

                None => match or_else {
                    Some(value) => Ok(Some(value.clone())),
                    None => Err(ctx.throw(
                        args_at.index,
                        format!(
                            "index '{index}' is out-of-bounds (list only contains {} elements)",
                            items.len()
                        ),
                    )),
                },
            }
        },
    )
}
