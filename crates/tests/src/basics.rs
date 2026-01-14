use reshell_builtins::type_helpers::IntType;

use crate::{
    run_expect_non_throw_error, run_expect_specific_value, run_expect_success, run_expect_throw,
};

#[test]
fn empty_program() {
    run_expect_success("");
    run_expect_success(" ");
    run_expect_success("\n");
    run_expect_success(&" \n".repeat(10));
}

#[test]
fn basic_math() {
    run_expect_specific_value::<IntType>(
        r#"
        let mut i = 2;
        assert ($i == 2)

        $i = $i + 1;
        assert ($i == 3)

        $i = $i * 2;
        assert ($i == 6)

        $i = $i / 3;
        assert ($i == 2)

        $i = $i % $i;
        assert ($i == 0)

        $i
    "#,
        0,
    );
}

#[test]
fn number_boundaries() {
    run_expect_success("-9223372036854775808");
    run_expect_success("9223372036854775807");
}

#[test]
fn out_of_bounds_numbers() {
    run_expect_throw("1 / 0");
    run_expect_throw("0 / 0");
    run_expect_throw("-9223372036854775808 / -1");
    run_expect_throw("1.0 / 0.0");
}

#[test]
fn invalid_ops() {
    run_expect_non_throw_error("1 + 1.0");
    run_expect_non_throw_error("1.0 + 1");
    run_expect_non_throw_error("1.0 + 'a'");
    run_expect_non_throw_error("'a' + 1.0");
    run_expect_non_throw_error("1.0 + []");
    run_expect_non_throw_error("1.0 + 1..10");
    run_expect_non_throw_error("1.0 + 1..=10");
}

#[test]
fn ranged_loops() {
    run_expect_specific_value::<IntType>("let mut i = 0; for _ in 0..10 { $i = $i + 1 }; $i", 10);
    run_expect_specific_value::<IntType>("let mut i = 0; for _ in 1..10 { $i = $i + 1 }; $i", 9);
    run_expect_specific_value::<IntType>("let mut i = 0; for _ in 0..=10 { $i = $i + 1 }; $i", 11);
    run_expect_specific_value::<IntType>("let mut i = 0; for _ in 1..=10 { $i = $i + 1 }; $i", 10);
    run_expect_specific_value::<IntType>("let mut i = 0; for _ in 10..10 { $i = $i + 1 }; $i", 0);
    run_expect_specific_value::<IntType>("let mut i = 0; for _ in 10..=10 { $i = $i + 1 }; $i", 1);
    run_expect_specific_value::<IntType>("let mut i = 0; for _ in 11..10 { $i = $i + 1 }; $i", 0);
    run_expect_specific_value::<IntType>("let mut i = 0; for _ in 11..=10 { $i = $i + 1 }; $i", 0);
}

#[test]
fn while_loops() {
    run_expect_specific_value::<IntType>("let mut i = 0; while $i < 10 { $i = $i + 1 }; $i", 10);
    run_expect_specific_value::<IntType>("let mut i = 0; while $i <= 10 { $i = $i + 1 }; $i", 11);
    run_expect_specific_value::<IntType>("let mut i = 0; while $i > 0 { $i = $i + 1 }; $i", 0);
}
