use crate::define_internal_fn;

define_internal_fn!(
    "at",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        index: RequiredArg<ExactIntType<usize>> = Arg::positional("index"),
        or_else: OptionalArg<AnyType> = Arg::long_flag("or-else")
    )

    -> AnyType
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             list,
             index,
             or_else,
         },
         _,
         ctx| {
            let items = list.read_promise_no_write();

            match items.get(index) {
                Some(value) => Ok(Some(value.clone())),

                None => match or_else {
                    Some(value) => Ok(Some(value.clone())),
                    None => Err(ctx.throw(
                        at,
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
