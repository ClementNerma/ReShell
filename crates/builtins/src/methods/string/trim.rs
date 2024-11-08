use crate::define_internal_fn;

define_internal_fn!(
    "trim",

    (
        string: RequiredArg<StringType> = Arg::method_self()
    )

    -> Some(StringType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, _, _| Ok(Some(RuntimeValue::String(string.trim().to_owned()))))
}
