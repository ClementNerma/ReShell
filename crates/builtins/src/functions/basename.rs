use std::path::Path;

use crate::define_internal_fn;

define_internal_fn!(
    "basename",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> NullableType<StringType>
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, _, _| {
        Ok(Some(
            Path::new(&path)
                .file_name()
                .map_or(RuntimeValue::Null, |str| {
                    RuntimeValue::String(str.to_str().unwrap().to_owned())
                }),
        ))
    })
}
