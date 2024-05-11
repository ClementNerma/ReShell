use crate::define_internal_fn;

define_internal_fn!(
    "append",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        append: RequiredArg<UntypedListType> = Arg::positional("append")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { list, append }, at, ctx| {
        list.write(at.list, ctx)?
            .extend(append.read_promise_no_write().iter().cloned());

        Ok(None)
    })
}
