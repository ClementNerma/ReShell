use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature},
};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "any",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        predicate: RequiredArg<PredicateFn> = Arg::positional("predicate")
    )

    -> Some(BoolType::value_type())
);

declare_typed_fn_handler!(PredicateFn => forge_basic_fn_signature(
    vec![("value", AnyType::value_type())],
    Some(BoolType::value_type()),
));

fn run() -> Runner {
    Runner::new(|_, Args { list, predicate }, args_at, ctx| {
        let filter = LocatedValue::new(args_at.predicate, RuntimeValue::Function(predicate));

        let mut any = false;

        for value in list.read(args_at.predicate).iter() {
            let validated =
                call_fn_checked(&filter, &PredicateFn::signature(), vec![value.clone()], ctx)
                    .and_then(|ret| {
                        expect_returned_value::<_, BoolType>(ret, args_at.predicate, ctx)
                    })?;

            if validated {
                any = true;
                break;
            }
        }

        Ok(Some(RuntimeValue::Bool(any)))
    })
}
