use crate::define_internal_fn;

define_internal_fn!(
    "startsWith",

    (
        string: RequiredArg<StringType> = Arg::positional("self"),
        sub: RequiredArg<StringType> = Arg::positional("sub")
    )

    -> Some(BoolType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { string, sub }, _, _| {
        Ok(Some(RuntimeValue::Bool(string.starts_with(&sub))))
    })
}
