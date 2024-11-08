use crate::define_internal_fn;
use crate::functions::RegexValue;

define_internal_fn!(
    "matches",

    (
        regex: RequiredArg<CustomType<RegexValue>> = Arg::method_self(),
        subject: RequiredArg<StringType> = Arg::positional("subject")
    )

    -> Some(BoolType::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { regex, subject }, _, _| {
        Ok(Some(RuntimeValue::Bool(regex.inner().is_match(&subject))))
    })
}
