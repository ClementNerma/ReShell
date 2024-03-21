use std::{ffi::OsString, fs, path::PathBuf};

use colored::Colorize;

crate::define_internal_fn!(
    //
    // List items in a directory
    //

    "ls",

    (
        path: OptionalArg<StringType> = Arg::positional("path")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|at, Args { path }, ArgsAt { path: path_at, .. }, ctx| {
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

        let read_dir = fs::read_dir(&path)
            .map_err(|err| ctx.error(at, format!("Failed to read directory: {err}")))?;

        struct LsItem {
            name: OsString,
            item_type: LsItemType,
            // hidden: bool
        }

        #[derive(PartialEq, Eq, PartialOrd, Ord)]
        enum LsItemType {
            Directory,
            File { size: u64 },
            Symlink { target: PathBuf },
            Unknown,
        }

        let mut items = vec![];

        for item in read_dir {
            let item = item
                .map_err(|err| ctx.error(at, format!("Failed to read directory entry: {err}")))?;

            let metadata = fs::symlink_metadata(item.path()).map_err(|err| {
                ctx.error(
                    at,
                    format!(
                        "Failed to get metadata from item '{}': {err}",
                        item.path().display()
                    ),
                )
            })?;

            items.push(LsItem {
                name: item.file_name(),
                // hidden: item.path().starts_with("."),
                item_type: if metadata.is_dir() {
                    LsItemType::Directory
                } else if metadata.is_file() {
                    LsItemType::File {
                        size: metadata.len(),
                    }
                } else if metadata.is_symlink() {
                    LsItemType::Symlink {
                        target: fs::read_link(item.path()).map_err(|err| {
                            ctx.error(
                                at,
                                format!(
                                    "Failed to read symlink target from'{}': {err}",
                                    item.path().display()
                                ),
                            )
                        })?,
                    }
                } else {
                    LsItemType::Unknown
                },
            });
        }

        items.sort_by(|a, b| {
            a.item_type
                .cmp(&b.item_type)
                .then_with(|| a.name.cmp(&b.name))
        });

        for item in items {
            let LsItem { name, item_type } = item;

            let name = name.to_string_lossy();

            match item_type {
                LsItemType::Directory => {
                    println!("{}/", name.bright_blue());
                }

                LsItemType::File { size } => {
                    println!("{name} {}", format!("({size} bytes)").bright_cyan());
                }

                LsItemType::Symlink { target } => {
                    println!(
                        "{} {}",
                        name.bright_magenta(),
                        format!("-> {}", target.to_string_lossy()).bright_yellow()
                    );
                }

                LsItemType::Unknown => {
                    println!("{}", name.bright_red());
                }
            }
        }

        Ok(None)
    })
}
