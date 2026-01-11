use crate::define_internal_fn;

define_internal_fn!(
    // get value at the provided index of throw

    "expectAt",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        index: RequiredArg<ExactIntType<usize>> = Arg::positional("index"),
        err_msg: OptionalArg<StringType> = Arg::long_flag("err-msg")
    )

    -> AnyType
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             list,
             index,
             err_msg,
         },
         _,
         ctx| {
            let items = list.read_promise_no_write();

            match items.get(index) {
                Some(value) => Ok(Some(value.clone())),

                None => Err(ctx.throw(
                    at,
                    err_msg.unwrap_or_else(|| {
                        format!(
                            "index '{index}' is out-of-bounds (list only contains {} elements)",
                            items.len()
                        )
                    }),
                )),
            }
        },
    )
}
