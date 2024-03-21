use reshell_runtime::gc::GcCell;

use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "listMap",

    (
        list: RequiredArg<UntypedListType> = Arg::positional("list"),
        mapper: RequiredArg<TypedFunctionType> = mapper_type()
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn mapper_type() -> RequiredArg<TypedFunctionType> {
    RequiredArg::new(
        ArgNames::Positional("mapper"),
        TypedFunctionType::new(forge_basic_fn_signature(
            vec![
                ("index", ExactIntType::<usize>::direct_underlying_type()),
                ("value", AnyType::direct_underlying_type()),
            ],
            Some(AnyType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(
        |_,
         Args { list, mapper },
         ArgsAt {
             list: _,
             mapper: mapper_at,
         },
         ctx| {
            let mapper = LocatedValue::new(RuntimeValue::Function(mapper), mapper_at);

            let mapped = list
                .read(mapper_at)
                .iter()
                .enumerate()
                .map(|(index, value)| -> ExecResult<RuntimeValue> {
                    let ret = call_fn_checked(
                        &mapper,
                        mapper_type().base_typing().signature(),
                        vec![
                            RuntimeValue::Int(index.try_into().expect(
                                "list contains too many elements to be represented by an integer",
                            )),
                            value.clone(),
                        ],
                        ctx,
                    )?;

                    Ok(expect_returned_value(
                        ret,
                        mapper_at,
                        AnyType::new_direct(),
                        ctx,
                    ))
                })
                .collect::<Result<_, _>>()?;

            Ok(Some(RuntimeValue::List(GcCell::new(mapped))))
        },
    )
}
