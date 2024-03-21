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
            Ok(Some(RuntimeValue::String(div_round_str(
                divident, dividor, precision,
            ))))
        },
    )
}

pub fn div_round_str(a: u64, b: u64, mut precision: u8) -> String {
    let max_prec = 10_u128.pow(u32::from(precision));

    let div = u128::from(a) * max_prec / u128::from(b);

    let mut int_part = div / max_prec;
    let mut frac_part = div % max_prec;

    let last_digit = (u128::from(a) * max_prec * 10 / u128::from(b)) % 10;

    if last_digit > 5 {
        if precision == 0 {
            int_part += 1;
            frac_part = 0;
        } else {
            frac_part += 1;

            if frac_part >= max_prec {
                int_part += 1;
                frac_part = 0;
            }
        }
    }

    while frac_part % 10 == 0 {
        frac_part /= 10;
        precision -= 1;
    }

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
