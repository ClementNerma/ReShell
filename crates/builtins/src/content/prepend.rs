use reshell_runtime::gc::GcCell;

use crate::define_internal_fn;

define_internal_fn!(
    "prepend",

    (
        list: RequiredArg<UntypedListType> = Arg::positional("list"),
        prepend: RequiredArg<UntypedListType> = Arg::positional("prepend")
    )

    -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list, prepend }, _, _| {
        let list = list.with_ref(|list| {
            prepend.with_ref(|prepend| {
                let mut out = prepend.clone();
                out.extend(list.iter().cloned());
                out
            })
        });

        Ok(Some(RuntimeValue::List(GcCell::new(list))))
    })
}
