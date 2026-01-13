#[test]
fn empty_program() {
    use crate::run_expect_success;

    run_expect_success("");
    run_expect_success(" ");
    run_expect_success("\n");
    run_expect_success(&" \n".repeat(10));
}
