use crate::define_internal_fn;

define_internal_fn!(
    "prepend",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        prepend: RestArg<AnyType> = RestArg::new("prepend")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { list, prepend }, at, ctx| {
        list.write(at.list, ctx)?.splice(0..0, prepend);
        Ok(None)
    })
}
