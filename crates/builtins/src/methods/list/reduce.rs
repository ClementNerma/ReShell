use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value},
};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "reduce",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        reducer: RequiredArg<ReducerFn> = Arg::positional("reducer")
    )

    -> AnyType
);

declare_typed_fn_handler!(
    ReducerFn(acc: AnyType, value: ExactIntType<usize>) -> NullableType<AnyType>
);

fn run() -> Runner {
    Runner::new(|_, Args { list, reducer }, args_at, ctx| {
        let reducer = LocatedValue::new(args_at.reducer, RuntimeValue::Function(reducer));

        let list = list.read(args_at.reducer);

        let result = match list.first() {
            None => RuntimeValue::Null,
            Some(first) => {
                let mut acc = first.clone();

                for value in list.iter().skip(1) {
                    acc = call_fn_checked(
                        &reducer,
                        &ReducerFn::signature(),
                        vec![acc.clone(), value.clone()],
                        ctx,
                    )
                    .and_then(|ret| {
                        expect_returned_value::<_, AnyType>(ret, args_at.reducer, ctx)
                    })?;
                }

                acc
            }
        };

        Ok(Some(result))
    })
}
