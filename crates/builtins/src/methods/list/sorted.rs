use reshell_runtime::gc::GcCell;

use crate::declare_typed_union_handler;

crate::define_internal_fn!(
    //
    // sort a list's items into a new list
    //
    "sorted",

    (
        list: RequiredArg<ComparableListType> = Arg::method_self()
    )

     -> UntypedListType
);

declare_typed_union_handler!(pub ComparableListType => enum ComparableList {
    String(DetachedListType<StringType>),
    Int(DetachedListType<IntType>),
    Duration(DetachedListType<DurationType>),
    DateTime(DetachedListType<DateTimeType>)
});

fn run() -> Runner {
    Runner::new(|_, Args { list }, _, _| {
        let sorted = match list {
            ComparableList::String(mut strings) => {
                strings.sort();
                strings.into_iter().map(RuntimeValue::String).collect()
            }

            ComparableList::Int(mut integers) => {
                integers.sort();
                integers.into_iter().map(RuntimeValue::Int).collect()
            }

            ComparableList::Duration(mut durations) => {
                durations.sort();
                durations.into_iter().map(RuntimeValue::Duration).collect()
            }

            ComparableList::DateTime(mut datetimes) => {
                datetimes.sort();
                datetimes.into_iter().map(RuntimeValue::DateTime).collect()
            }
        };

        Ok(Some(RuntimeValue::List(GcCell::new(sorted))))
    })
}
