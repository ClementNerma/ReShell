use crate::{
    declare_typed_union_handler,
    helpers::types::{IntType, StringType},
    type_helpers::{DateTimeType, DurationType},
};

declare_typed_union_handler!(pub ComparableValueType => enum ComparableType {
    String(StringType),
    Int(IntType),
    Duration(DurationType),
    DateTime(DateTimeType)
});
