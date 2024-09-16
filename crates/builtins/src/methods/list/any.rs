use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "any",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        checker: RequiredArg<TypedFunctionType> = checker_type()
    )

    -> Some(BoolType::direct_underlying_type())
);

fn checker_type() -> RequiredArg<TypedFunctionType> {
    Arg::new(
        ArgNames::Positional("checker"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![("value", AnyType::direct_underlying_type())],
            Some(BoolType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_,
         Args { list, checker },
         ArgsAt {
             list: _,
             checker: checker_at,
         },
         ctx| {
            let filter = LocatedValue::new(checker_at, RuntimeValue::Function(checker));

            let mut any = false;

            for value in list.read(checker_at).iter() {
                let validated = call_fn_checked(
                    &filter,
                    checker_type().base_typing().signature(),
                    vec![value.clone()],
                    ctx,
                )
                .and_then(|ret| {
                    expect_returned_value(ret, checker_at, BoolType::new_direct(), ctx)
                })?;

                if validated {
                    any = true;
                    break;
                }
            }

            Ok(Some(RuntimeValue::Bool(any)))
        },
    )
}