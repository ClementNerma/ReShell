use reshell_runtime::gc::GcCell;

use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value},
};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "filterMap",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        filter_map: RequiredArg<FilterMapFn> = Arg::positional("filterMap")
    )

    -> UntypedListType
);

declare_typed_fn_handler!(
    FilterMapFn(value: AnyType) -> NullableType<AnyType>
);

fn run() -> Runner {
    Runner::new(|_, Args { list, filter_map }, args_at, ctx| {
        let filter_map = LocatedValue::new(args_at.filter_map, RuntimeValue::Function(filter_map));

        let mut filter_mapped = vec![];

        for value in list.read(args_at.filter_map).iter() {
            let result = call_fn_checked(
                &filter_map,
                &FilterMapFn::signature(),
                vec![value.clone()],
                ctx,
            )
            .and_then(|ret| expect_returned_value::<_, AnyType>(ret, args_at.filter_map, ctx))?;

            if !matches!(result, RuntimeValue::Null) {
                filter_mapped.push(result);
            }
        }

        Ok(Some(RuntimeValue::List(GcCell::new(filter_mapped))))
    })
}
