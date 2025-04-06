use compiler::compile_program;
use serde::Deserialize;
use serde_json::from_str;

const A1: &str = include_str!("./assignments/a4/test1.l");
const A2: &str = include_str!("./assignments/a4/test2.l");
const A3: &str = include_str!("./assignments/a4/test3.l");
const A4: &str = include_str!("./assignments/a4/test4.l");
const A5: &str = include_str!("./assignments/a4/test5.l");
const A6: &str = include_str!("./assignments/a4/test6.l");
const A7: &str = include_str!("./assignments/a4/test7.l");
const A8: &str = include_str!("./assignments/a4/test8.l");
const A9: &str = include_str!("./assignments/a4/test9.l");
const A10: &str = include_str!("./assignments/a4/test10.l");
const A11: &str = include_str!("./assignments/a4/test11.l");

const TESTS: &[&str] = &[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11];

#[derive(Deserialize)]
struct TestExpectedResult {
    compiles: bool
}

fn read_expectations(src_text: &str) -> TestExpectedResult {
    let json_src = &src_text[3..src_text.find('\n').unwrap()];
    from_str::<TestExpectedResult>(json_src).unwrap()
}

#[test]
fn test_assignment_4_tests() {
    for (test, i) in TESTS.iter().zip(1..) {
        println!("Assgnment 4 test {}", i);
        let expected_result = read_expectations(test);

        let c_res = compile_program(test);
        if expected_result.compiles && c_res.is_err() {
            panic!("Program fails to compile but should succeed. Compile errors: {:?}", c_res.err().unwrap())
        }
        if !expected_result.compiles {
            continue
        }
    }
}
