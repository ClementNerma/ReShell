use glob::{glob_with, MatchOptions};
use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // Select items using a pattern
    //

    "glob",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern"),
        case_sensitive: OptionalArg<BoolType> = Arg::long_flag("case-sensitive"),
        lossy: OptionalArg<BoolType> = Arg::long_flag("lossy")
    )

    -> Some(DetachedListType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             pattern,
             case_sensitive,
             lossy,
         },
         ArgsAt {
             pattern: pattern_at,
             ..
         },
         ctx| {
            let mut options = MatchOptions::default();

            if case_sensitive == Some(true) {
                options.case_sensitive = true;
            }

            let paths = glob_with(&pattern, options).map_err(|err| {
                ctx.error(pattern_at, format!("invalid glob pattern provided: {err}"))
            })?;

            let paths = paths
                .map(|path| {
                    path.map_err(|err| {
                        ctx.error(at, format!("failed to access path during glob: {err}"))
                    })
                    .and_then(|path| {
                        if lossy == Some(true) {
                            Ok(RuntimeValue::String(path.to_string_lossy().to_string()))
                        } else {
                            match path.to_str() {
                                Some(path) => Ok(RuntimeValue::String(path.to_owned())),
                                None => Err(ctx.error(
                                    at,
                                    format!(
                                        "encountered path with invalid UTF-8 character(s): {}",
                                        path.display()
                                    ),
                                )),
                            }
                        }
                    })
                })
                .collect::<Result<_, _>>()?;

            Ok(Some(RuntimeValue::List(GcCell::new(paths))))
        },
    )
}
