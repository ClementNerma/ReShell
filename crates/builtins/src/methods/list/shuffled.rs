use rand::{seq::SliceRandom, thread_rng};
use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // put a list's items in a random order into a new list
    //

    "shuffled",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

    -> Some(UntypedListType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { list }, _, _| {
        let mut items = list.read_promise_no_write().clone();
        items.shuffle(&mut thread_rng());

        Ok(Some(RuntimeValue::List(GcCell::new(items))))
    })
}
