mod utils;

use utils::test_files;

const TESTS: &[&str] = &[
    include_str!("./assignments/additional/not.l"),
    include_str!("./assignments/additional/anonymous_subtuple.l"),
    include_str!("./assignments/additional/anonymous_subtuple_2.l"),
];

#[test]
fn run_additional_tests() {
    test_files(TESTS)
}
