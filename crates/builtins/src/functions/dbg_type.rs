use colored::Colorize;
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};
use terminal_size::{terminal_size, Width};

crate::define_internal_fn!(
    //
    // Debug a value's type
    //

    "dbgType",

    (
        value: RequiredArg<AnyType> = Arg::positional("value"),
        tab_size: OptionalArg<ExactIntType<usize>> = Arg::long_flag("tab-size"),
        max_line_size: OptionalArg<ExactIntType<usize>> = Arg::long_flag("max-line-size")
    )

    -> VoidType
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             value,
             tab_size,
             max_line_size,
         },
         _,
         ctx| {
            let at = format!(
                "dbgType [{}]:",
                at.render_colored(ctx.files_map(), PrettyPrintOptions::inline())
            );

            println!(
                "{} {}",
                at.bright_magenta(),
                value.compute_type().render_colored(
                    ctx.type_alias_store(),
                    PrettyPrintOptions {
                        pretty: true,
                        line_prefix_size: at.chars().count(),
                        max_line_size: max_line_size
                            .or_else(|| terminal_size().map(|(Width(width), _)| usize::from(width)))
                            .unwrap_or(30),
                        tab_size: tab_size.unwrap_or(4)
                    }
                )
            );

            Ok(None)
        },
    )
}
