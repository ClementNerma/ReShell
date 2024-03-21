use crate::define_internal_fn;

define_internal_fn!(
    "trim",

    (
        string: RequiredArg<StringType> = Arg::positional("self")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string }, _, _| Ok(Some(RuntimeValue::String(string.trim().to_owned()))))
}
