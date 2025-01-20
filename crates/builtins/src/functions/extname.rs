use std::path::Path;

use crate::define_internal_fn;

define_internal_fn!(
    "extname",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> NullableType<StringType>
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, _, _| {
        Ok(Some(match Path::new(&path).extension() {
            Some(ext) => RuntimeValue::String(ext.to_str().unwrap().to_owned()),
            None => RuntimeValue::Null,
        }))
    })
}
