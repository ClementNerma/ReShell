use reshell_runtime::gc::GcCell;

use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value},
};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "map",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        mapper: RequiredArg<MapperFn> = Arg::positional("mapper")
    )

    -> Some(UntypedListType::value_type())
);

declare_typed_fn_handler!(MapperFn(value: AnyType) -> AnyType);

fn run() -> Runner {
    Runner::new(|_, Args { list, mapper }, args_at, ctx| {
        let mapper = LocatedValue::new(args_at.mapper, RuntimeValue::Function(mapper));

        let mapped = list
            .read(args_at.mapper)
            .iter()
            .map(|value| {
                call_fn_checked(&mapper, &MapperFn::signature(), vec![value.clone()], ctx)
                    .and_then(|ret| expect_returned_value::<_, AnyType>(ret, args_at.mapper, ctx))
            })
            .collect::<Result<_, _>>()?;

        Ok(Some(RuntimeValue::List(GcCell::new(mapped))))
    })
}
