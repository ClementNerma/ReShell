use reshell_runtime::gc::GcCell;

use crate::define_internal_fn;
use crate::functions::RegexValue;

define_internal_fn!(
    "captures",

    (
        regex: RequiredArg<CustomType<RegexValue>> = Arg::method_self(),
        subject: RequiredArg<StringType> = Arg::positional("subject")
    )

    -> Some(NullableType::<DetachedMapType::<StringType>>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { regex, subject }, _, _| {
        let ret = match regex.inner().captures(&subject) {
            None => RuntimeValue::Null,
            Some(captured) => {
                let mut counter = 0;
                let mut names = regex.inner().capture_names();

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
