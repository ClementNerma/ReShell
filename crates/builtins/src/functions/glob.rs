use reshell_globby::{Globby, GlobbyOptions};
use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // Select items using a pattern
    //

    "glob",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern"),
        case_insensitive: PresenceFlag = Arg::long_flag("case-insensitive"),
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> DetachedListType<StringType>
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             pattern,
             case_insensitive,
             lossy,
         },
         args_at,
         ctx| {
            let options = GlobbyOptions {
                case_insensitive,
                ..Default::default()
            };

            let paths = Globby::in_current_dir(&pattern, options).map_err(|err| {
                ctx.throw(
                    args_at.pattern,
                    format!("invalid glob pattern provided: {err}"),
                )
            })?;

            let paths = paths
                .map(|entry| -> ExecResult<RuntimeValue> {
                    let entry = entry.map_err(|err| {
                        ctx.throw(at, format!("failed to access path during glob: {err}"))
                    })?;

                    let path = entry.path();

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
