use reshell_builtins::type_helpers::StringType;

use crate::{run_expect_non_throw_error, run_expect_specific_value, run_expect_throw};

#[test]
fn approx_int_div() {
    // Basic
    run_expect_specific_value::<StringType>("approxIntDiv 1 2 --precision 0", "1");
    run_expect_specific_value::<StringType>("approxIntDiv 2 2 --precision 0", "1");
    run_expect_specific_value::<StringType>("approxIntDiv 3 2 --precision 0", "2");

    // Negative (left)
    run_expect_specific_value::<StringType>("approxIntDiv -1 2 --precision 0", "-1");
    run_expect_specific_value::<StringType>("approxIntDiv -2 2 --precision 0", "-1");
    run_expect_specific_value::<StringType>("approxIntDiv -3 2 --precision 0", "-2");
    run_expect_specific_value::<StringType>("approxIntDiv -1 2 --precision 1", "-0.5");

    // Negative (right)
    run_expect_specific_value::<StringType>("approxIntDiv 1 -2 --precision 0", "-1");
    run_expect_specific_value::<StringType>("approxIntDiv 2 -2 --precision 0", "-1");
    run_expect_specific_value::<StringType>("approxIntDiv 3 -2 --precision 0", "-2");
    run_expect_specific_value::<StringType>("approxIntDiv 1 -2 --precision 1", "-0.5");

    // Double negative
    run_expect_specific_value::<StringType>("approxIntDiv -1 -2 --precision 0", "1");
    run_expect_specific_value::<StringType>("approxIntDiv -2 -2 --precision 0", "1");
    run_expect_specific_value::<StringType>("approxIntDiv -3 -2 --precision 0", "2");
    run_expect_specific_value::<StringType>("approxIntDiv -1 -2 --precision 1", "0.5");

    // Rounding
    run_expect_specific_value::<StringType>("approxIntDiv 1 2 --precision 0", "1");
    run_expect_specific_value::<StringType>("approxIntDiv 1 2 --precision 0", "1");

    // Truncating
    run_expect_specific_value::<StringType>("approxIntDiv 1 2 --precision 2", "0.5");
    run_expect_specific_value::<StringType>("approxIntDiv 0 2 --precision 2", "0");

    // Arguments variation
    run_expect_specific_value::<StringType>("approxIntDiv 1 2 -p 1", "0.5");

    // Invalid
    run_expect_throw("approxIntDiv 0 0 --precision 3");
    run_expect_throw("approxIntDiv 1 0 --precision 3");

    // Invalid arguments
    run_expect_non_throw_error("approxIntDiv 1.0 0 --precision 1");
    run_expect_non_throw_error("approxIntDiv 0 1.0 --precision 1");
    run_expect_non_throw_error("approxIntDiv 1 1 --precision a");
    run_expect_non_throw_error("approxIntDiv 1 1 -p a");
    run_expect_non_throw_error("approxIntDiv 1 -p 2");
    run_expect_non_throw_error("approxIntDiv 1 1");
    run_expect_non_throw_error("approxIntDiv 1");
    run_expect_non_throw_error("approxIntDiv");
}
