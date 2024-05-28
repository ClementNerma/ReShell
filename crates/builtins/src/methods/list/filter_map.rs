use reshell_runtime::gc::GcCell;

use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "filterMap",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        filter: RequiredArg<TypedFunctionType> = filter_type()
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn filter_type() -> RequiredArg<TypedFunctionType> {
    Arg::new(
        ArgNames::Positional("filter"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![(
                "value",
                Union2Type::<AnyType, NullType>::direct_underlying_type(),
            )],
            Some(BoolType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_,
         Args { list, filter },
         ArgsAt {
             list: _,
             filter: filter_at,
         },
         ctx| {
            let filter = LocatedValue::new(RuntimeValue::Function(filter), filter_at);

            let mut filtered = vec![];

            for value in list.read(filter_at).iter() {
                let result = call_fn_checked(
                    &filter,
                    filter_type().base_typing().signature(),
                    vec![value.clone()],
                    ctx,
                )
                .and_then(|ret| {
                    expect_returned_value(ret, filter_at, AnyType::new_direct(), ctx)
                })?;

                if !matches!(result, RuntimeValue::Null) {
                    filtered.push(result);
                }
            }

            Ok(Some(RuntimeValue::List(GcCell::new(filtered))))
        },
    )
}
