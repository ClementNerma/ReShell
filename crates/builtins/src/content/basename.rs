use std::path::Path;

use crate::define_internal_fn;

define_internal_fn!(
    "basename",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> Some(NullableType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, _, _| {
        Ok(Some(match Path::new(&path).components().last() {
            Some(component) => {
                RuntimeValue::String(component.as_os_str().to_str().unwrap().to_owned())
            }
            None => RuntimeValue::Null,
        }))
    })
}
