use colored::Colorize;
use reshell_runtime::{
    display::dbg_loc,
    pretty::{PrettyPrintOptions, PrettyPrintable},
};
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

    -> None
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
                dbg_loc(at, ctx.files_map()).bright_yellow()
            );

            println!(
                "{} {}",
                at.bright_magenta(),
                value.get_type().render_colored(
                    ctx,
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
