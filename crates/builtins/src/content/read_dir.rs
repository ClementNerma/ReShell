use std::{fs, path::PathBuf};

use reshell_runtime::{errors::ExecErrorInfoType, gc::GcCell};

crate::define_internal_fn!(
    //
    // List items in a directory
    //

    "readDir",

    (
        path: OptionalArg<StringType> = Arg::positional("path"),
        lossy: OptionalArg<BoolType> = Arg::long_flag("lossy")
    )

    -> Some(DetachedListType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |at, Args { path, lossy }, ArgsAt { path: path_at, .. }, ctx| {
            let path = match path {
                None => std::env::current_dir().map_err(|err| {
                    ctx.error(
                        at,
                        format!("Failed to get path to current directory: {err}"),
                    )
                })?,

                Some(path) => {
                    let path = PathBuf::from(path);

                    if !path.is_dir() {
                        return Err(ctx.error(
                            path_at.unwrap(),
                            format!("Directory not found at path: {}", path.display()),
                        ));
                    }

                    path
                }
            };

            let read_dir = fs::read_dir(path)
                .map_err(|err| ctx.error(at, format!("Failed to read directory: {err}")))?;

            let mut items = vec![];

            for item in read_dir {
                let item = item.map_err(|err| {
                    ctx.error(at, format!("Failed to read directory entry: {err}"))
                })?;

                let path = if lossy == Some(true) {
                    item.path()
                        .to_str()
                        .ok_or_else(|| {
                            ctx.error(
                                at,
                                format!(
                                    "Item name contains invalid UTF-8 characters: {}",
                                    item.path().display()
                                ),
                            )
                            .with_info(
                                ExecErrorInfoType::Tip,
                                "you can use '--lossy' to get the string anyway",
                            )
                        })?
                        .to_owned()
                } else {
                    item.path().to_string_lossy().into_owned()
                };

                items.push(RuntimeValue::String(path));
            }

            Ok(Some(RuntimeValue::List(GcCell::new(items))))
        },
    )
}
