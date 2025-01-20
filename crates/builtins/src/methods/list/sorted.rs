use reshell_runtime::gc::{GcCell, GcReadOnlyCell};

use crate::types::{DateTimeValue, DurationValue};

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

pub type ComparableValueType =
    Union4Type<StringType, IntType, CustomType<DurationValue>, CustomType<DateTimeValue>>;

pub type ComparableListType = Union4Type<
    DetachedListType<StringType>,
    DetachedListType<IntType>,
    DetachedListType<CustomType<DurationValue>>,
    DetachedListType<CustomType<DateTimeValue>>,
>;

fn run() -> Runner {
    Runner::new(|_, Args { list }, _, _| Ok(Some(RuntimeValue::List(GcCell::new(sort_list(list))))))
}

pub fn sort_list(list: <ComparableListType as TypedValueParser>::Parsed) -> Vec<RuntimeValue> {
    match list {
        Union4Result::A(mut strings) => {
            strings.sort();

            strings.into_iter().map(RuntimeValue::String).collect()
        }

        Union4Result::B(mut integers) => {
            integers.sort();

            integers.into_iter().map(RuntimeValue::Int).collect()
        }

        Union4Result::C(mut durations) => {
            durations.sort();

            durations
                .into_iter()
                .map(|val| RuntimeValue::Custom(GcReadOnlyCell::new(val)))
                .collect()
        }

        Union4Result::D(mut datetimes) => {
            datetimes.sort();
            datetimes
                .into_iter()
                .map(|val| RuntimeValue::Custom(GcReadOnlyCell::new(val)))
                .collect()
        }
    }
}
