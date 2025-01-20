use crate::define_internal_fn;

define_internal_fn!(
    "append",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        append: RestArg<AnyType> = RestArg::new("append")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|_, Args { list, append }, at, ctx| {
        list.write(at.list, ctx)?.extend(append);
        Ok(None)
    })
}
