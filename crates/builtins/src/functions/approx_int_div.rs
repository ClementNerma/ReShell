crate::define_internal_fn!(
    //
    // Display a message
    //

    "approxIntDiv",

    (
        divident: RequiredArg<IntType> = Arg::positional("divident"),
        dividor: RequiredArg<IntType> = Arg::positional("dividor"),
        precision: RequiredArg<ExactIntType<u8>> = Arg::long_and_short_flag("precision", 'p')
    )

    -> StringType
);

fn run() -> Runner {
    Runner::new(
        |at,
         Args {
             divident,
             dividor,
             precision,
         },
         _,
         ctx| {
            let str =
                approx_int_div(divident, dividor, precision).map_err(|err| ctx.throw(at, err))?;

            Ok(Some(RuntimeValue::String(str)))
        },
    )
}

/// Perform an approximate integer division
///
/// The last decimal will be rounded to the nearest.
///
/// The `precision` parameter is the number of floating-point decimals to keep.
pub fn approx_int_div(a: i64, b: i64, precision: u8) -> Result<String, &'static str> {
    if b == 0 {
        return Err("attempting to divide by zero");
    }

    let is_a_neg = a.is_negative();
    let is_b_neg = b.is_negative();

    let a = a.abs();
    let b = b.abs();

    let max_prec = 10_i128.pow(u32::from(precision));

    let mut div = i128::from(a) * max_prec * 10 / i128::from(b);
    div = div / 10
        + if div.is_negative() && div % 10 < 5 {
            -1
        } else if !div.is_negative() && div % 10 >= 5 {
            1
        } else {
            0
        };

    let int_part = div / max_prec;
    let frac_part = div % max_prec;

    let mut out = format!("{}{int_part}", if is_a_neg ^ is_b_neg { "-" } else { "" });

    if frac_part > 0 && precision > 0 {
        out.push('.');
        out.push_str(&format!(
            "{:#0precision$}",
            frac_part,
            precision = precision.into()
        ));

        if out.ends_with('0') {
            let first_non_zero = out.rfind(|c| c != '0').unwrap();
            out.truncate(first_non_zero + 1);
        }

        assert!(!out.ends_with('.'));
    }

    Ok(out)
}
