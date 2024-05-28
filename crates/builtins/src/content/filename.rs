use std::path::Path;

use crate::define_internal_fn;

define_internal_fn!(
    "filename",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> Some(NullableType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, _, _| {
        Ok(Some(match Path::new(&path).file_name() {
            Some(name) => RuntimeValue::String(name.to_str().unwrap().to_owned()),
            None => RuntimeValue::Null,
        }))
    })
}
