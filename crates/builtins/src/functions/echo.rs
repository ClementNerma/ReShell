use colored::{Color, Colorize};

use crate::{
    declare_string_literal_type,
    methods::{StringifyableType, stringify_value},
};

crate::define_internal_fn!(
    //
    // Display a message
    //

    "echo",

    (
        message: RequiredArg<StringifyableType> = Arg::positional("message"),
        color: OptionalArg<ColorNameType> = Arg::long_and_short_flag("color", 'c'),
        no_newline: PresenceFlag = Arg::long_and_short_flag("no-newline", 'n')
    )

    -> VoidType
);

declare_string_literal_type!(
    ColorNameType => enum ColorName {
        Black ("black"),
        Red ("red"),
        Green ("green"),
        Yellow ("yellow"),
        Blue ("blue"),
        Magenta ("magenta"),
        Cyan ("cyan"),
        White ("white"),
        BrightBlack ("brightBlack"),
        BrightRed ("brightRed"),
        BrightGreen ("brightGreen"),
        BrightYellow ("brightYellow"),
        BrightBlue ("brightBlue"),
        BrightMagenta ("brightMagenta"),
        BrightCyan ("brightCyan"),
        BrightWhite ("brightWhite")
    }
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             message,
             color,
             no_newline,
         },
         _,
         _| {
            let message = stringify_value(message);

            match color {
                None => println!("{message}"),

                Some(color) => {
                    let color = match color {
                        ColorName::Black => Color::Black,
                        ColorName::Red => Color::Red,
                        ColorName::Green => Color::Green,
                        ColorName::Yellow => Color::Yellow,
                        ColorName::Blue => Color::Blue,
                        ColorName::Magenta => Color::Magenta,
                        ColorName::Cyan => Color::Cyan,
                        ColorName::White => Color::White,

                        ColorName::BrightBlack => Color::BrightBlack,
                        ColorName::BrightRed => Color::BrightRed,
                        ColorName::BrightGreen => Color::BrightGreen,
                        ColorName::BrightYellow => Color::BrightYellow,
                        ColorName::BrightBlue => Color::BrightBlue,
                        ColorName::BrightMagenta => Color::BrightMagenta,
                        ColorName::BrightCyan => Color::BrightCyan,
                        ColorName::BrightWhite => Color::BrightWhite,
                    };

                    let message = message.color(color);

                    if no_newline {
                        print!("{message}");
                    } else {
                        println!("{message}");
                    }
                }
            }

            Ok(None)
        },
    )
}
