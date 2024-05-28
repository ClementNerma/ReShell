use reshell_runtime::gc::GcCell;

use crate::define_internal_fn;

define_internal_fn!(
    "prepend",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        prepend: RequiredArg<UntypedListType> = Arg::positional("prepend")
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list, prepend }, _, _| {
        let mut out = prepend.read_promise_no_write().clone();
        out.extend(list.read_promise_no_write().iter().cloned());

        Ok(Some(RuntimeValue::List(GcCell::new(out))))
    })
}
