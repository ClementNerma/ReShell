crate::define_internal_fn!(
    //
    // Display a message
    //

    "approxIntDiv",

    (
        divident: RequiredArg<ExactIntType<u64>> = Arg::positional("divident"),
        dividor: RequiredArg<ExactIntType<u64>> = Arg::positional("dividor"),
        precision: RequiredArg<ExactIntType<u8>> = Arg::long_and_short_flag("precision", 'p')
    )

    -> None
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             divident,
             dividor,
             precision,
         },
         _,
         _| {
            Ok(Some(RuntimeValue::String(approx_int_div(
                divident, dividor, precision,
            ))))
        },
    )
}

pub fn approx_int_div(a: u64, b: u64, precision: u8) -> String {
    let max_prec = 10_u128.pow(u32::from(precision));

    let div = u128::from(a) * max_prec * 10 / u128::from(b);
    let div = (div / 10) + if div % 10 >= 5 { 1 } else { 0 };

    let int_part = div / max_prec;
    let frac_part = div % max_prec;

    format!(
        "{int_part}{}",
        if frac_part > 0 && precision > 0 {
            format!(
                ".{:#0precision$}",
                frac_part,
                precision = usize::from(precision)
            )
        } else {
            String::new()
        }
    )
}
