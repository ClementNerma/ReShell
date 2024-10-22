use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "all",

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
    Runner::new(|_, Args { list, checker }, args_at, ctx| {
        let filter = LocatedValue::new(args_at.checker, RuntimeValue::Function(checker));

        let mut all = true;

        for value in list.read(args_at.checker).iter() {
            let validated = call_fn_checked(
                &filter,
                checker_type().base_typing().signature(),
                vec![value.clone()],
                ctx,
            )
            .and_then(|ret| {
                expect_returned_value(ret, args_at.checker, BoolType::new_direct(), ctx)
            })?;

            if !validated {
                all = false;
                break;
            }
        }

        Ok(Some(RuntimeValue::Bool(all)))
    })
}
