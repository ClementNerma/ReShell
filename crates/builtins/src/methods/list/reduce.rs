use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "reduce",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        reduce_fn: RequiredArg<TypedFunctionType> = reduce_fn_type()
    )

    -> Some(AnyType::direct_underlying_type())
);

fn reduce_fn_type() -> RequiredArg<TypedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("reduce_fn"),
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
         Args { list, reduce_fn },
         ArgsAt {
             reduce_fn: reduce_fn_at,
             ..
         },
         ctx| {
            let reduce_fn = LocatedValue::new(reduce_fn_at, RuntimeValue::Function(reduce_fn));

            let list = list.read(reduce_fn_at);

            let result = match list.first() {
                None => RuntimeValue::Null,
                Some(first) => {
                    let mut acc = first.clone();

                    for value in list.iter().skip(1) {
                        acc = call_fn_checked(
                            &reduce_fn,
                            reduce_fn_type().base_typing().signature(),
                            vec![acc.clone(), value.clone()],
                            ctx,
                        )
                        .and_then(|ret| {
                            expect_returned_value(ret, reduce_fn_at, AnyType::new_direct(), ctx)
                        })?;
                    }

                    acc
                }
            };

            Ok(Some(result))
        },
    )
}
