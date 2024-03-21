use crate::content::stringify::stringify_value;

use super::stringify::StringifyableType;

crate::define_internal_fn!(
    //
    // Display a message
    //

    "echo",

    (
        message: RequiredArg<StringifyableType> = Arg::positional("message")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args { message }, _, _| {
        println!("{}", stringify_value(message));

        Ok(None)
    })
}
