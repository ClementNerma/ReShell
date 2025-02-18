use reshell_runtime::gc::GcCell;

use crate::types::RegexValue;

crate::define_internal_fn!(
    "capture",

    (
        regex: RequiredArg<CustomType<RegexValue>> = Arg::method_self(),
        subject: RequiredArg<StringType> = Arg::positional("subject")
    )

    -> NullableType<DetachedMapType<StringType>>
);

fn run() -> Runner {
    Runner::new(|_, Args { regex, subject }, _, _| {
        let ret = match regex.captures(&subject) {
            None => RuntimeValue::Null,
            Some(captured) => {
                let mut counter = 0;
                let mut names = regex.capture_names();

                let map = captured
                    .iter()
                    .flatten()
                    .map(|matched| {
                        let name = match names.next().unwrap() {
                            Some(name) => name.to_owned(),
                            None => {
                                counter += 1;
                                (counter - 1).to_string()
                            }
                        };

                        (name, RuntimeValue::String(matched.as_str().to_owned()))
                    })
                    .collect();

                RuntimeValue::Map(GcCell::new(map))
            }
        };

        Ok(Some(ret))
    })
}
