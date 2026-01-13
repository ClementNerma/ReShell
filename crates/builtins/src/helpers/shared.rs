use crate::{
    declare_typed_union_handler,
    helpers::types::{CustomType, IntType, StringType},
    types::{DateTimeValue, DurationValue},
};

declare_typed_union_handler!(pub ComparableValueType => enum ComparableType {
    String(StringType),
    Int(IntType),
    Duration(CustomType<DurationValue>),
    DateTime(CustomType<DateTimeValue>)
});
