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
        let list = list.with_ref(|list| {
            append.with_ref(|append| {
                let mut out = list.clone();
                out.extend(append.iter().cloned());
                out
            })
        });

        Ok(Some(RuntimeValue::List(GcCell::new(list))))
    })
}
