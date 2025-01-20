use reshell_runtime::gc::GcCell;

use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature},
};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "filter",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        predicate: RequiredArg<PredicateFn> = Arg::positional("predicate")
    )

    -> Some(UntypedListType::value_type())
);

declare_typed_fn_handler!(PredicateFn => forge_basic_fn_signature(
    vec![("value", AnyType::value_type())],
    Some(BoolType::value_type()),
));

fn run() -> Runner {
    Runner::new(|_, Args { list, predicate }, args_at, ctx| {
        let predicate = LocatedValue::new(args_at.predicate, RuntimeValue::Function(predicate));

        let mut filtered = vec![];

        for value in list.read(args_at.predicate).iter() {
            let keep = call_fn_checked(
                &predicate,
                &PredicateFn::signature(),
                vec![value.clone()],
                ctx,
            )
            .and_then(|ret| expect_returned_value::<_, BoolType>(ret, args_at.predicate, ctx))?;

            if keep {
                filtered.push(value.clone());
            }
        }

        Ok(Some(RuntimeValue::List(GcCell::new(filtered))))
    })
}
