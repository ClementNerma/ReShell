use terminal_size::{Height, Width};

crate::define_internal_fn!(
    //
    // Get the current number of colums of the terminal
    //

    "termRows",

    ()

    -> Some(NullableType::<ExactIntType<usize>>::value_type())
);

fn run() -> Runner {
    Runner::new(|_, _, _, _| {
        let cols = match terminal_size::terminal_size() {
            Some((Width(_), Height(height))) => RuntimeValue::Int(height.into()),
            None => RuntimeValue::Null,
        };

        Ok(Some(cols))
    })
}
