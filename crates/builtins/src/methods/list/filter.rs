use reshell_runtime::gc::GcCell;

use crate::utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "filter",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        filter: RequiredArg<SignatureBasedFunctionType> = filter_type()
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn filter_type() -> RequiredArg<SignatureBasedFunctionType> {
    Arg::new(
        ArgNames::Positional("filter"),
        SignatureBasedFunctionType::new(forge_basic_fn_signature(
            vec![("value", AnyType::direct_underlying_type())],
            Some(BoolType::direct_underlying_type()),
        )),
    )
}

fn run() -> Runner {
    Runner::new(|_, Args { list, filter }, args_at, ctx| {
        let filter = LocatedValue::new(args_at.filter, RuntimeValue::Function(filter));

        let mut filtered = vec![];

        for value in list.read(args_at.filter).iter() {
            let keep = call_fn_checked(
                &filter,
                filter_type().base_typing().signature(),
                vec![value.clone()],
                ctx,
            )
            .and_then(|ret| {
                expect_returned_value(ret, args_at.filter, BoolType::new_direct(), ctx)
            })?;

            if keep {
                filtered.push(value.clone());
            }
        }

        Ok(Some(RuntimeValue::List(GcCell::new(filtered))))
    })
}
