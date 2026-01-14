use colored::Colorize;
use parsy::FileId;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::{
    context::ScopeContent, pretty_impl::pretty_printable_code_range, values::RuntimeFnBody,
};

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
                                    pretty_printable_code_range(at, ctx.files_map())
                                        .display(&(), PrettyPrintOptions::inline())
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
                                pretty_printable_code_range(at, ctx.files_map())
                                    .display(&(), PrettyPrintOptions::inline())
                                    .to_string()
                                    .underline()
                            }

                            RuntimeCodeRange::Internal(str) => {
                                format!("internal location: {str}").italic()
                            }
                        },
                        func.value
                            .signature
                            .inner()
                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    );

                    if let RuntimeFnBody::Block(block) = &func.value.body
                        && let FileId::SourceFile(id) = block.at.start.file_id
                    {
                        let source = ctx.files_map().get_file(id).unwrap().content;

                        println!(
                            "\n{}",
                            source[block.at.start.offset..block.at.start.offset + block.at.len]
                                .italic()
                        );
                    }

                    return Ok(None);
                } else if let Some(cmd_alias) = cmd_aliases.get(&command) {
                    let at = cmd_alias.value.content.at;

                    println!(
                        "Alias declared at: {}\n\n{}",
                        pretty_printable_code_range(
                            cmd_alias.value.name_declared_at,
                            ctx.files_map()
                        )
                        .display(&(), PrettyPrintOptions::inline())
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
