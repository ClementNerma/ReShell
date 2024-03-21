use crate::functions::RegexValue;

crate::define_internal_fn!(
    //
    // replace substrings
    //

    "replace",

    (
        source: RequiredArg<StringType> = Arg::method_self(),
        replacer: RequiredArg<Union2Type<StringType, CustomType<RegexValue>>> = Arg::positional("replacer"),
        replacement: RequiredArg<StringType> = Arg::positional("replacement")
    )

    -> Some(StringType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             source,
             replacer,
             replacement,
         },
         _,
         _| {
            let replaced = match replacer {
                Union2Result::A(string) => source.replace(&string, &replacement),
                Union2Result::B(regex) => regex.inner().replace(&source, &replacement).into_owned(),
            };

            Ok(Some(RuntimeValue::String(replaced)))
        },
    )
}
