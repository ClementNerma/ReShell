use crate::declare_typed_union_handler;

crate::define_internal_fn!(
    //
    // Display a message
    //

    "toString",

    (
        value: RequiredArg<StringifyableType> = Arg::method_self()
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| Ok(Some(RuntimeValue::String(stringify_value(value)))))
}

declare_typed_union_handler!(pub StringifyableType => enum Stringifyable {
    String(StringType),
    Int(IntType),
    Float(FloatType),
    Bool(BoolType)
});

pub fn stringify_value(value: <StringifyableType as TypedValueParser>::Parsed) -> String {
    match value {
        Stringifyable::String(string) => string,
        Stringifyable::Int(int) => int.to_string(),
        Stringifyable::Float(float) => float.to_string(),
        Stringifyable::Bool(bool) => bool.to_string(),
    }
}
