use glob::{glob_with, MatchOptions};
use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // Select items using a pattern
    //

    "glob",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern"),
        case_sensitive: PresenceFlag = Arg::long_flag("case-sensitive"),
        lossy: PresenceFlag = Arg::long_flag("lossy")
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

            if case_sensitive {
                options.case_sensitive = true;
            }

            let paths = glob_with(&pattern, options).map_err(|err| {
                ctx.throw(pattern_at, format!("invalid glob pattern provided: {err}"))
            })?;

            let paths = paths
                .map(|path| -> ExecResult<RuntimeValue> {
                    let path = path.map_err(|err| {
                        ctx.throw(at, format!("failed to access path during glob: {err}"))
                    })?;

                    if lossy {
                        return Ok(RuntimeValue::String(path.to_string_lossy().to_string()));
                    }

                    let path = path.to_str().ok_or_else(|| {
                        ctx.throw(
                            at,
                            format!(
                                "encountered path with invalid UTF-8 character(s): {}",
                                path.display()
                            ),
                        )
                    })?;

                    Ok(RuntimeValue::String(path.to_owned()))
                })
                .collect::<Result<_, _>>()?;

            Ok(Some(RuntimeValue::List(GcCell::new(paths))))
        },
    )
}
