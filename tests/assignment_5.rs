mod utils;

use utils::test_files;

const TESTS: &[&str] = &[
    include_str!("./assignments/a5/test1.l"),
    include_str!("./assignments/a5/test2.l"),
    include_str!("./assignments/a5/test3.l"),
    include_str!("./assignments/a5/test4.l"),
    include_str!("./assignments/a5/test5.l"),
    include_str!("./assignments/a5/test6.l"),
];

#[test]
fn test_assignment_5_tests() {
    test_files(TESTS)
}
