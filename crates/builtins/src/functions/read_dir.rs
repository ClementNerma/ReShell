use std::{
    fs,
    path::{Path, PathBuf},
};

use reshell_runtime::{errors::ExecInfoType, gc::GcCell};

use crate::declare_string_literal_type;

crate::define_internal_fn!(
    //
    // List items in a directory
    //

    "readDir",

    (
        dir_path: OptionalArg<StringType> = Arg::positional("dirPath"),
        full_path: PresenceFlag = Arg::long_and_short_flag("full-path", 'p'),
        relative_path: PresenceFlag = Arg::long_and_short_flag("relative-path", 'r'),
        lossy: PresenceFlag = Arg::long_flag("lossy")
    )

    -> DetachedListType<StringType>
);

declare_string_literal_type!(PathModeType => enum PathMode {
    FullPath("full-path"),
    Relative("relative")
});

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             dir_path,
             full_path,
             relative_path,
             lossy,
         },
         args_at,
         ctx| {
            if full_path && relative_path {
                return Err(ctx.throw(
                    at,
                    "Cannot use both '--full-path' and '--relative-path' at the same time",
                ));
            }

            let reading_dir = match dir_path {
                None => std::env::current_dir().map_err(|err| {
                    ctx.throw(
                        at,
                        format!("Failed to get path to current directory: {err}"),
                    )
                })?,

                Some(ref path) => {
                    let path = PathBuf::from(path.to_owned());

                    if !path.is_dir() {
                        return Err(ctx.throw(
                            args_at.dir_path.unwrap(),
                            format!("Directory not found at path: {}", path.display()),
                        ));
                    }

                    path
                }
            };

            let read_dir = fs::read_dir(&reading_dir).map_err(|err| {
                ctx.throw(
                    at,
                    format!(
                        "Failed to read directory '{}': {err}",
                        reading_dir.display()
                    ),
                )
            })?;

            let mut items = vec![];

            for item in read_dir {
                let item = item.map_err(|err| {
                    ctx.throw(
                        at,
                        format!(
                            "Failed to read directory entry '{}': {err}",
                            reading_dir.display()
                        ),
                    )
                })?;

                let item_path = if full_path {
                    let canon = fs::canonicalize(item.path()).map_err(|err| {
                        ctx.error(
                            args_at.full_path,
                            format!(
                                "Failed to canonicalize directory entry at '{}': {}",
                                item.path().display(),
                                err
                            ),
                        )
                    })?;

                    canon.into_os_string()
                } else if relative_path {
                    match &dir_path {
                        Some(dir_path) => {
                            Path::new(dir_path).join(item.file_name()).into_os_string()
                        }
                        None => item.file_name().to_os_string(),
                    }
                } else {
                    item.file_name()
                };

                let item_path = if lossy {
                    item_path
                        .to_str()
                        .ok_or_else(|| {
                            ctx.throw(
                                at,
                                format!(
                                    "Path contains invalid UTF-8 characters: {}",
                                    item_path.display()
                                ),
                            )
                            .with_info(
                                ExecInfoType::Tip,
                                "you can use '--lossy' to get the string anyway",
                            )
                        })?
                        .to_owned()
                } else {
                    item_path.to_string_lossy().into_owned()
                };

                items.push(RuntimeValue::String(item_path));
            }

            Ok(Some(RuntimeValue::List(GcCell::new(items))))
        },
    )
}
