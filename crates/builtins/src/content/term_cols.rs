use terminal_size::{Height, Width};

crate::define_internal_fn!(
    //
    // Get the current number of colums of the terminal
    //

    "term_cols",

    ()

    -> Some(NullableType::<ExactIntType<usize>>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, _, _, _| {
        let cols = match terminal_size::terminal_size() {
            Some((Width(width), Height(_))) => RuntimeValue::Int(i64::try_from(width).unwrap()),
            None => RuntimeValue::Null,
        };

        Ok(Some(cols))
    })
}
