crate::define_internal_fn!(
    //
    // Display a message
    //

    "toString",

    (
        value: RequiredArg<StringifyableType> = Arg::positional("value")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| Ok(Some(RuntimeValue::String(stringify_value(value)))))
}

pub type StringifyableType = Union4Type<StringType, IntType, FloatType, BoolType>;

pub fn stringify_value(value: <StringifyableType as Typing>::Parsed) -> String {
    match value {
        Union4Result::A(string) => string,
        Union4Result::B(int) => int.to_string(),
        Union4Result::C(float) => float.to_string(),
        Union4Result::D(bool) => bool.to_string(),
    }
}
