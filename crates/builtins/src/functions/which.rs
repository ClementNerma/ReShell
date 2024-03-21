use colored::Colorize;
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
                    cmd_aliases,
                } = scope;

                if let Some(func) = fns.get(&command) {
                    println!(
                        "{}",
                        func.value.render_colored(ctx, PrettyPrintOptions::inline())
                    );

                    return Ok(None);
                }

                if let Some(cmd_alias) = cmd_aliases.get(&command) {
                    println!(
                        "alias declared at {}",
                        dbg_loc(cmd_alias.value.name_declared_at, ctx.files_map())
                    );

                    return Ok(None);
                }
            }
        }

        match ctx.binaries_resolver().resolve_binary_path(&command) {
            Ok(path) => {
                println!("External binary: {}", path.display());
            }

            Err(err) => {
                eprintln!("{}", err.bright_red());
            }
        }

        Ok(None)
    })
}
