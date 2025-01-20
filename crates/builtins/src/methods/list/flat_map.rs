use reshell_runtime::gc::GcCell;

use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value},
};

crate::define_internal_fn!(
    //
    // map over a list and flatten the results
    //

    "flatMap",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        mapper: RequiredArg<MapperFn> = Arg::positional("mapper")
    )

    -> Some(UntypedListType::value_type())
);

declare_typed_fn_handler!(MapperFn(value: AnyType) -> UntypedListType);

fn run() -> Runner {
    Runner::new(|_, Args { list, mapper }, args_at, ctx| {
        let mapper = LocatedValue::new(args_at.mapper, RuntimeValue::Function(mapper));

        let mut flattened = vec![];

        for value in list.read(args_at.mapper).iter() {
            let value = call_fn_checked(&mapper, &MapperFn::signature(), vec![value.clone()], ctx)?;

            let value = expect_returned_value::<_, AnyType>(value, args_at.mapper, ctx)?;

            match value {
                RuntimeValue::List(list) => {
                    flattened.extend(list.read_promise_no_write().iter().cloned());
                }

                _ => flattened.push(value),
            }
        }

        Ok(Some(RuntimeValue::List(GcCell::new(flattened))))
    })
}
