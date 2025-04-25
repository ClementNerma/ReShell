use colored::Colorize;
use parsy::FileId;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::context::ScopeContent;

use crate::define_internal_fn;

define_internal_fn!(
    "which",

    (
        command: RequiredArg<StringType> = Arg::positional("command"),
        external: PresenceFlag = Arg::long_and_short_flag("external", 'e')
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(|_, Args { command, external }, _, ctx| {
        if !external {
            for scope in ctx.visible_scopes_content() {
                let ScopeContent {
                    vars: _,
                    fns,
                    methods,
                    cmd_aliases,
                } = scope;

                if let Some(methods) = command
                    .strip_prefix('.')
                    .and_then(|method_name| methods.get(method_name))
                {
                    for method in methods {
                        println!(
                            "* Method declared at: {}\n  {}\n",
                            match method.name_at {
                                RuntimeCodeRange::Parsed(at) => {
                                    at.display(ctx.files_map(), PrettyPrintOptions::inline())
                                        .to_string()
                                        .underline()
                                }

                                RuntimeCodeRange::Internal(str) => {
                                    format!("internal location: {str}").italic()
                                }
                            },
                            method.value.display(ctx, PrettyPrintOptions::inline())
                        );
                    }

                    return Ok(None);
                } else if let Some(func) = fns.get(&command) {
                    println!(
                        "Function declared at: {}\n\n{}",
                        match func.name_at {
                            RuntimeCodeRange::Parsed(at) => {
                                at.display(ctx.files_map(), PrettyPrintOptions::inline())
                                    .to_string()
                                    .underline()
                            }

                            RuntimeCodeRange::Internal(str) => {
                                format!("internal location: {str}").italic()
                            }
                        },
                        func.value.display(ctx, PrettyPrintOptions::inline())
                    );

                    return Ok(None);
                } else if let Some(cmd_alias) = cmd_aliases.get(&command) {
                    let at = cmd_alias.value.content.at;

                    println!(
                        "Alias declared at: {}\n\n{}",
                        cmd_alias
                            .value
                            .name_declared_at
                            .display(ctx.files_map(), PrettyPrintOptions::inline())
                            .to_string()
                            .underline(),
                        match at.start.file_id {
                            FileId::SourceFile(id) => {
                                let source = ctx.files_map().get_file(id).unwrap().content;
                                source[at.start.offset..at.start.offset + at.len].italic()
                            }

                            _ => "<no source available>".italic(),
                        }
                    );

                    return Ok(None);
                }
            }
        }

        match ctx.binaries_resolver().resolve_binary_path(&command) {
            Ok(path) => {
                println!("External binary at: {}", path.to_string_lossy().underline());
            }

            Err(err) => {
                eprintln!("{}", err.bright_red());
            }
        }

        Ok(None)
    })
}
