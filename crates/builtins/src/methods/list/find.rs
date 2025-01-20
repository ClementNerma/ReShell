use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value},
};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "find",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        predicate: RequiredArg<PredicateFn> = Arg::positional("predicate")
    )

    -> Union2Type<AnyType, NullType>
);

declare_typed_fn_handler!(PredicateFn(value: Union2Type<AnyType, NullType>) -> BoolType);

fn run() -> Runner {
    Runner::new(|_, Args { list, predicate }, args_at, ctx| {
        let predicate = LocatedValue::new(args_at.predicate, RuntimeValue::Function(predicate));

        for value in list.read(args_at.predicate).iter() {
            let keep = call_fn_checked(
                &predicate,
                &PredicateFn::signature(),
                vec![value.clone()],
                ctx,
            )
            .and_then(|ret| expect_returned_value::<_, BoolType>(ret, args_at.predicate, ctx))?;

            if keep {
                return Ok(Some(value.clone()));
            }
        }

        Ok(Some(RuntimeValue::Null))
    })
}
