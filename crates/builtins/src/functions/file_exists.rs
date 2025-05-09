use std::path::Path;

crate::define_internal_fn!(
    //
    // Check if a path exists
    //

    "fileExists",

    (
        path: RequiredArg<StringType> = Arg::positional("path")
    )

    -> BoolType
);

fn run() -> Runner {
    Runner::new(|_, Args { path }, _, _| Ok(Some(RuntimeValue::Bool(Path::new(&path).is_file()))))
}
