mod utils;

use utils::test_files;

const TESTS: &[&str] = &[
    include_str!("./assignments/a7/test1.l"),
];

#[test]
fn test_assignment_7_tests() {
    test_files(TESTS)
}
