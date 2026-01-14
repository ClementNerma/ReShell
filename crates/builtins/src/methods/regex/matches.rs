crate::define_internal_fn!(
    "matches",

    (
        regex: RequiredArg<RegexType> = Arg::method_self(),
        subject: RequiredArg<StringType> = Arg::positional("subject")
    )

    -> BoolType
);

fn run() -> Runner {
    Runner::new(|_, Args { regex, subject }, _, _| {
        Ok(Some(RuntimeValue::Bool(regex.is_match(&subject))))
    })
}
