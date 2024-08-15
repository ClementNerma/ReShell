use std::cmp::Ordering;

use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // sort a list's items into a new list
    //
    "sorted",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

     -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, args_at, ctx| {
        let list = list.read_promise_no_write();

        let sorted_list =
        // integers
        try_sort_type(
            &list,
            |item| match item {
                RuntimeValue::Int(int) => Some(*int),
                _ => None,
            },
            RuntimeValue::Int,
            i64::cmp,
        )
        // floats
        .or_else(|| {
            try_sort_type(
                &list,
                |item| match item {
                    RuntimeValue::Float(float) => Some(*float),
                    _ => None,
                },
                RuntimeValue::Float,
                |a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal),
            )
        })
        // strings
        .or_else(|| {
            try_sort_type(
                &list,
                |item| match item {
                    RuntimeValue::String(string) => Some(string.clone()),
                    _ => None,
                },
                RuntimeValue::String,
                String::cmp,
            )
        })
        // failure
        .ok_or_else(|| {
            ctx.throw(args_at.list, "only integer, floating-point and string lists can be sorted")
        })?;

        Ok(Some(sorted_list))
    })
}

fn try_sort_type<T>(
    list: &[RuntimeValue],
    extract: impl Fn(&RuntimeValue) -> Option<T>,
    remap: fn(T) -> RuntimeValue,
    sort_by: impl Fn(&T, &T) -> Ordering,
) -> Option<RuntimeValue> {
    let mut items = list
        .iter()
        .map(|item| extract(item).ok_or(()))
        .collect::<Result<Vec<_>, ()>>()
        .ok()?;

    items.sort_by(sort_by);

    Some(RuntimeValue::List(GcCell::new(
        items.into_iter().map(remap).collect(),
    )))
}
