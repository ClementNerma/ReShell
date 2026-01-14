use std::path::{Path, PathBuf};

use globby::{Pattern, PatternOpts, Walker};
use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // Select items using a pattern
    //

    "glob",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern"),
        from: OptionalArg<StringType> = Arg::long_and_short_flag("from", 'f'),
        case_sensitive: PresenceFlag = Arg::long_flag("case-sensitive"),
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> DetachedListType<StringType>
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             pattern,
             from,
             case_sensitive,
             lossy,
         },
         args_at,
         ctx| {
            let pattern = Pattern::new_with_opts(
                &pattern,
                PatternOpts {
                    case_insensitive: !case_sensitive,
                },
            )
            .map_err(|err| {
                ctx.throw(
                    args_at.pattern,
                    format!("invalid glob pattern provided: {err:?}"),
                )
            })?;

            let from = match from {
                Some(from) => {
                    if !Path::new(&from).is_dir() {
                        return Err(ctx.throw(
                            args_at.from.unwrap(),
                            format!("provided directory does not exist at path: {from}"),
                        ));
                    }

                    PathBuf::from(from)
                }
                None => std::env::current_dir().map_err(|err| {
                    ctx.hard_error(
                        at,
                        format!("Failed to get path to current directory: {err}"),
                    )
                })?,
            };

            let paths = Walker::new(pattern, &from)
                .map(|entry| -> ExecResult<RuntimeValue> {
                    let path = entry.map_err(|err| {
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
