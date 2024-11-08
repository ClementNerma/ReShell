use std::path::Path;

use crate::define_internal_fn;

define_internal_fn!(
    "parentDir",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> Some(NullableType::<StringType>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, _, _| {
        Ok(Some(match Path::new(&path).parent() {
            Some(parent) => RuntimeValue::String(parent.to_str().unwrap().to_owned()),
            None => RuntimeValue::Null,
        }))
    })
}
