use reshell_runtime::gc::GcCell;

use crate::define_internal_fn;

define_internal_fn!(
    "append",

    (
        list: RequiredArg<UntypedListType> = Arg::positional("list"),
        append: RequiredArg<UntypedListType> = Arg::positional("append")
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list, append }, _, _| {
        let mut out = list.read_promise_no_write().clone();
        out.extend(append.read_promise_no_write().iter().cloned());

        Ok(Some(RuntimeValue::List(GcCell::new(out))))
    })
}
