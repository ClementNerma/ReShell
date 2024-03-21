crate::define_internal_fn!(
    //
    // Display a message
    //

    "stringify",

    (
        value: RequiredArg<StringifyableType> = Arg::positional("value")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { value }, _, _| Ok(Some(RuntimeValue::String(stringify_value(value)))))
}

pub type StringifyableType = Union3Type<StringType, IntType, FloatType>;

pub fn stringify_value(value: <StringifyableType as Typing>::Parsed) -> String {
    match value {
        Union3Result::A(string) => string,
        Union3Result::B(int) => int.to_string(),
        Union3Result::C(float) => float.to_string(),
    }
}
