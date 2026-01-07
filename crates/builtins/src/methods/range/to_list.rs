use reshell_runtime::{gc::GcCell, values::RangeValue};

use crate::define_internal_fn;

define_internal_fn!(
    "toList",

    (
        range: RequiredArg<RangeType> = Arg::method_self()
    )

    -> DetachedListType<IntType>
);

fn run() -> Runner {
    Runner::new(|_, Args { range }, _, _| {
        let RangeValue {
            from,
            to,
            include_last_value,
        } = range;

        let range = from..to + if include_last_value { 1 } else { 0 };

        Ok(Some(RuntimeValue::List(GcCell::new(
            range.map(RuntimeValue::Int).collect(),
        ))))
    })
}
