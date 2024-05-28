use crate::utils::{call_fn_checked, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "each",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        for_each_fn: RequiredArg<TypedFunctionType> = for_each_fn_type()
    )

    -> None
);

fn for_each_fn_type() -> RequiredArg<TypedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("for_each_fn"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![("value", AnyType::direct_underlying_type())],
            Some(AnyType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_,
         Args { list, for_each_fn },
         ArgsAt {
             list: list_at,
             for_each_fn: for_each_fn_at,
         },
         ctx| {
            let for_each_fn =
                LocatedValue::new(RuntimeValue::Function(for_each_fn), for_each_fn_at);

            for value in list.read(list_at).iter() {
                call_fn_checked(
                    &for_each_fn,
                    for_each_fn_type().base_typing().signature(),
                    vec![value.clone()],
                    ctx,
                )?;
            }

            Ok(None)
        },
    )
}
