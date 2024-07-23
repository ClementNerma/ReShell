use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "fold",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        init: RequiredArg<AnyType> = Arg::positional("init"),
        fold_fn: RequiredArg<TypedFunctionType> = fold_fn_type()
    )

    -> Some(AnyType::direct_underlying_type())
);

fn fold_fn_type() -> RequiredArg<TypedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("fold_fn"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![
                ("acc", AnyType::direct_underlying_type()),
                ("value", ExactIntType::<usize>::direct_underlying_type()),
            ],
            Some(NullableType::<AnyType>::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             list,
             init,
             fold_fn,
         },
         ArgsAt {
             fold_fn: fold_fn_at,
             ..
         },
         ctx| {
            let fold_fn = LocatedValue::new(fold_fn_at, RuntimeValue::Function(fold_fn));

            let list = list.read(fold_fn_at);

            let mut folded = init.clone();

            for value in list.iter() {
                folded = call_fn_checked(
                    &fold_fn,
                    fold_fn_type().base_typing().signature(),
                    vec![folded.clone(), value.clone()],
                    ctx,
                )
                .and_then(|ret| {
                    expect_returned_value(ret, fold_fn_at, AnyType::new_direct(), ctx)
                })?;
            }

            Ok(Some(folded))
        },
    )
}
