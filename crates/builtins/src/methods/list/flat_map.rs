use reshell_runtime::gc::GcCell;

use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list and flatten the results
    //

    "flatMap",

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
            Some(UntypedListType::direct_underlying_type()),
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

            let mut flattened = vec![];

            for value in list.read(mapper_at).iter() {
                let value = call_fn_checked(
                    &mapper,
                    mapper_type().base_typing().signature(),
                    vec![value.clone()],
                    ctx,
                )?;

                let value = expect_returned_value(value, mapper_at, AnyType::new_direct(), ctx)?;

                match value {
                    RuntimeValue::List(list) => {
                        flattened.extend(list.read_promise_no_write().iter().cloned());
                    }

                    _ => flattened.push(value)
                }
            }

            Ok(Some(RuntimeValue::List(GcCell::new(flattened))))
        },
    )
}
