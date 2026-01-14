use std::sync::Arc;

use reshell_runtime::gc::GcCell;

use crate::{
    declare_typed_union_handler,
    types::{DateTimeValue, DurationValue},
};

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
    Duration(DetachedListType<CustomType<DurationValue>>),
    DateTime(DetachedListType<CustomType<DateTimeValue>>)
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

                durations
                    .into_iter()
                    .map(|val| RuntimeValue::Custom(Arc::new(val)))
                    .collect()
            }

            ComparableList::DateTime(mut datetimes) => {
                datetimes.sort();

                datetimes
                    .into_iter()
                    .map(|val| RuntimeValue::Custom(Arc::new(val)))
                    .collect()
            }
        };

        Ok(Some(RuntimeValue::List(GcCell::new(sorted))))
    })
}
