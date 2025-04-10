mod utils;

use utils::test_files;

const TESTS: &[&str] = &[
    include_str!("./assignments/a4/test1.l"),
    include_str!("./assignments/a4/test2.l"),
    include_str!("./assignments/a4/test3.l"),
    include_str!("./assignments/a4/test4.l"),
    include_str!("./assignments/a4/test5.l"),
    include_str!("./assignments/a4/test6.l"),
    include_str!("./assignments/a4/test7.l"),
    include_str!("./assignments/a4/test8.l"),
    include_str!("./assignments/a4/test9.l"),
    include_str!("./assignments/a4/test10.l"),
    include_str!("./assignments/a4/test11.l"),
];

#[test]
fn test_assignment_4_tests() {
    test_files(TESTS)
}
