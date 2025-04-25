mod utils;

use utils::test_files;

const TESTS: &[&str] = &[
    include_str!("./assignments/a7/test1.l"),
    include_str!("./assignments/a7/test2.l"),
    include_str!("./assignments/a7/test3.l"),
    include_str!("./assignments/a7/test4.l"),
    include_str!("./assignments/a7/test5.l"),
];

#[test]
fn test_assignment_7_tests() {
    test_files(TESTS)
}
