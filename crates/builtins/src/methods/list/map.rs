use reshell_runtime::gc::GcCell;

use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "map",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        mapper: RequiredArg<TypedFunctionType> = mapper_type()
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn mapper_type() -> RequiredArg<TypedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("mapper"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![("value", AnyType::direct_underlying_type())],
            Some(AnyType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_,
         Args { list, mapper },
         ArgsAt {
             mapper: mapper_at, ..
         },
         ctx| {
            let mapper = LocatedValue::new(mapper_at, RuntimeValue::Function(mapper));

            let mapped = list
                .read(mapper_at)
                .iter()
                .map(|value| {
                    call_fn_checked(
                        &mapper,
                        mapper_type().base_typing().signature(),
                        vec![value.clone()],
                        ctx,
                    )
                    .and_then(|ret| {
                        expect_returned_value(ret, mapper_at, AnyType::new_direct(), ctx)
                    })
                })
                .collect::<Result<_, _>>()?;

            Ok(Some(RuntimeValue::List(GcCell::new(mapped))))
        },
    )
}
