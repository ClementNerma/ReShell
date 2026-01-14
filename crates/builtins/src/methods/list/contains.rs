use reshell_runtime::values::{NotComparableTypeErr, are_values_equal};

use crate::define_internal_fn;

define_internal_fn!(
    "contains",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        value: RequiredArg<AnyType> = Arg::positional("value")
    )

    -> BoolType
);

fn run() -> Runner {
    Runner::new(|at, Args { list, value }, _, ctx| {
        let list = list.read_promise_no_write();

        for item in list.iter() {
            match are_values_equal(item, &value) {
                Ok(true) => return Ok(Some(RuntimeValue::Bool(true))),
                Ok(false) => {}
                Err(NotComparableTypeErr { reason }) => return Err(ctx.throw(at, reason)),
            }
        }

        Ok(Some(RuntimeValue::Bool(false)))
    })
}
