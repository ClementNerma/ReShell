use colored::{Color, Colorize};

use crate::functions::stringify::stringify_value;

use super::stringify::StringifyableType;

crate::define_internal_fn!(
    //
    // Display a message
    //

    "echo",

    (
        message: RequiredArg<StringifyableType> = Arg::positional("message"),
        color: OptionalArg<StringType> = Arg::long_and_short_flag("color", 'c')
    )

    -> None
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args { message, color },
         ArgsAt {
             color: color_at, ..
         },
         ctx| {
            let message = stringify_value(message);

            match color {
                None => println!("{message}"),

                Some(color) => {
                    let color = match color.as_str() {
                        "black" => Color::Black,
                        "red" => Color::Red,
                        "green" => Color::Green,
                        "yellow" => Color::Yellow,
                        "blue" => Color::Blue,
                        "magenta" => Color::Magenta,
                        "cyan" => Color::Cyan,
                        "white" => Color::White,
                        "brightBlack" => Color::BrightBlack,
                        "brightRed" => Color::BrightRed,
                        "brightGreen" => Color::BrightGreen,
                        "brightYellow" => Color::BrightYellow,
                        "brightBlue" => Color::BrightBlue,
                        "brightmagenta" => Color::BrightMagenta,
                        "brightCyan" => Color::BrightCyan,
                        "brightWhite" => Color::BrightWhite,
                        _ => {
                            return Err(
                                ctx.throw(color_at.unwrap(), format!("unknown color '{color}'"))
                            )
                        }
                    };

                    println!("{}", message.color(color));
                }
            }

            Ok(None)
        },
    )
}
