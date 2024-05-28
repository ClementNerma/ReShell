use reshell_runtime::gc::GcCell;

use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "filter",

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
            vec![
                ("value", AnyType::direct_underlying_type()),
                ("index", ExactIntType::<usize>::direct_underlying_type()),
            ],
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

            for (index, value) in list.read(filter_at).iter().enumerate() {
                let ret = call_fn_checked(
                    &filter,
                    filter_type().base_typing().signature(),
                    vec![
                        value.clone(),
                        RuntimeValue::Int(index.try_into().expect(
                            "list contains too many elements to be represented by an integer",
                        )),
                    ],
                    ctx,
                )?;

                let keep = expect_returned_value(ret, filter_at, BoolType::new_direct(), ctx);

                if keep {
                    filtered.push(value.clone());
                }
            }

            Ok(Some(RuntimeValue::List(GcCell::new(filtered))))
        },
    )
}
