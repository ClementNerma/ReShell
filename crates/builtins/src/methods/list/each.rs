use crate::{declare_typed_fn_handler, utils::call_fn_checked};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "each",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        for_each_fn: RequiredArg<ForEachFn> = Arg::positional("for_each_fn")
    )

    -> None
);

declare_typed_fn_handler!(ForEachFn(value: AnyType) -> AnyType);

fn run() -> Runner {
    Runner::new(|_, Args { list, for_each_fn }, args_at, ctx| {
        let for_each_fn =
            LocatedValue::new(args_at.for_each_fn, RuntimeValue::Function(for_each_fn));

        for value in list.read(args_at.list).iter() {
            call_fn_checked(
                &for_each_fn,
                &ForEachFn::signature(),
                vec![value.clone()],
                ctx,
            )?;
        }

        Ok(None)
    })
}
