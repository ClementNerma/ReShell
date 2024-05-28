use crate::define_internal_fn;

define_internal_fn!(
    "prepend",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        prepend: RequiredArg<UntypedListType> = Arg::positional("prepend")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { list, prepend }, at, ctx| {
        list.write(at.list, ctx)?
            .splice(0..0, prepend.read_promise_no_write().clone());

        Ok(None)
    })
}
