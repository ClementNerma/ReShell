use terminal_size::{Height, Width};

crate::define_internal_fn!(
    //
    // Get the current number of colums of the terminal
    //

    "termCols",

    ()

    -> Some(NullableType::<ExactIntType<usize>>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, _, _, _| {
        let cols = match terminal_size::terminal_size() {
            Some((Width(width), Height(_))) => RuntimeValue::Int(width.into()),
            None => RuntimeValue::Null,
        };

        Ok(Some(cols))
    })
}
