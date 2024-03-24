use std::{fs, path::PathBuf};

use reshell_runtime::{errors::ExecInfoType, gc::GcCell};

crate::define_internal_fn!(
    //
    // List items in a directory
    //

    "readDir",

    (
        path: OptionalArg<StringType> = Arg::positional("path"),
        lossy: PresenceFlag = Arg::long_flag("lossy"),
        full_path: PresenceFlag = Arg::long_and_short_flag("full-path", 'f')
    )

    -> Some(DetachedListType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             path,
             lossy,
             full_path,
         },
         ArgsAt { path: path_at, .. },
         ctx| {
            let reading_dir = match path {
                None => std::env::current_dir().map_err(|err| {
                    ctx.throw(
                        at,
                        format!("Failed to get path to current directory: {err}"),
                    )
                })?,

                Some(path) => {
                    let path = PathBuf::from(path);

                    if !path.is_dir() {
                        return Err(ctx.throw(
                            path_at.unwrap(),
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

                let path = if full_path {
                    item.path()
                } else {
                    item.path().strip_prefix(&reading_dir).unwrap().to_owned()
                };

                let path = if lossy {
                    path.to_str()
                        .ok_or_else(|| {
                            ctx.throw(
                                at,
                                format!(
                                    "Item name contains invalid UTF-8 characters: {}",
                                    path.display()
                                ),
                            )
                            .with_info(
                                ExecInfoType::Tip,
                                "you can use '--lossy' to get the string anyway",
                            )
                        })?
                        .to_owned()
                } else {
                    path.to_string_lossy().into_owned()
                };

                items.push(RuntimeValue::String(path));
            }

            Ok(Some(RuntimeValue::List(GcCell::new(items))))
        },
    )
}
