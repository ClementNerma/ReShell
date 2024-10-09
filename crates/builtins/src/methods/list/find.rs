use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "find",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        finder: RequiredArg<TypedFunctionType> = finder_type()
    )

    -> Some(Union2Type::<AnyType, NullType>::direct_underlying_type())
);

fn finder_type() -> RequiredArg<TypedFunctionType> {
    Arg::new(
        ArgNames::Positional("finder"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![("value", AnyType::direct_underlying_type())],
            Some(BoolType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_,
         Args { list, finder },
         ArgsAt {
             list: _,
             finder: finder_at,
         },
         ctx| {
            let finder = LocatedValue::new(finder_at, RuntimeValue::Function(finder));

            for value in list.read(finder_at).iter() {
                let keep = call_fn_checked(
                    &finder,
                    finder_type().base_typing().signature(),
                    vec![value.clone()],
                    ctx,
                )
                .and_then(|ret| {
                    expect_returned_value(ret, finder_at, BoolType::new_direct(), ctx)
                })?;

                if keep {
                    return Ok(Some(value.clone()));
                }
            }

            Ok(Some(RuntimeValue::Null))
        },
    )
}
