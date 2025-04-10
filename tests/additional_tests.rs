mod utils;

use utils::test_files;

const TESTS: &[&str] = &[
    include_str!("./assignments/additional/not.l"),
];

#[test]
fn run_additional_tests() {
    test_files(TESTS)
}
