use colored::Colorize;
use parsy::FileId;
use reshell_runtime::{
    context::ScopeContent,
    display::dbg_loc,
    pretty::{PrettyPrintOptions, PrettyPrintable},
};

use crate::define_internal_fn;

define_internal_fn!(
    "which",

    (
        command: RequiredArg<StringType> = Arg::positional("command"),
        direct: PresenceFlag = Arg::long_and_short_flag("direct", 'd')
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { command, direct }, _, ctx| {
        if !direct {
            for scope in ctx.visible_scopes_content() {
                let ScopeContent {
                    vars: _,
                    fns,
                    methods: _,
                    cmd_aliases,
                } = scope;

                if let Some(func) = fns.get(&command) {
                    // println!(
                    //     "Function declared at: {}\n",
                    //     dbg_loc(func, ctx.files_map()).underline()
                    // );

                    println!(
                        "Function declared at: {}\n\n{}",
                        match func.name_declared_at {
                            RuntimeCodeRange::Parsed(at) => {
                                dbg_loc(at, ctx.files_map()).underline()
                            }

                            RuntimeCodeRange::Internal(str) => {
                                format!("internal location: {str}").italic()
                            }
                        },
                        func.value.render_colored(ctx, PrettyPrintOptions::inline())
                    );

                    return Ok(None);
                }

                if let Some(cmd_alias) = cmd_aliases.get(&command) {
                    let at = cmd_alias.value.content.at;

                    println!(
                        "Alias declared at: {}\n\n{}",
                        dbg_loc(cmd_alias.value.name_declared_at, ctx.files_map()).underline(),
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
