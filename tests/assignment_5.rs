use std::path::Path;
use compiler::compile_program;
use std::process::Command;
use serde::Deserialize;
use serde_json::from_str;

#[derive(Deserialize)]
struct TestExpectedResult {
    pub compiles: bool,
    pub stdout: Option<String>,
}

fn read_expectations(src_text: &str) -> TestExpectedResult {
    let json_src = &src_text[3..src_text.find('\n').unwrap()];
    from_str::<TestExpectedResult>(json_src).unwrap()
}

fn link_runtime(prgm_assembly: &Path) {
    let mut link_cmd = Command::new("gcc");
    link_cmd.args([
        "-g",
        "runtime.o",
        prgm_assembly.as_os_str().to_str().unwrap(),
        "-o",
        "test_bin",
    ]);
    let out = link_cmd.output().unwrap();
    assert_eq!(
        out.status.code().unwrap(),
        0,
        "Failed to link \n\n{}",
        String::from_utf8(out.stderr).unwrap()
    );

    let mut chmod = Command::new("chmod");
    chmod.args(["+x", "test_bin"]);
    chmod.output().expect("Failed to chmod +x the file");
}

struct RunResult {
    stdout: String,
}

fn run_artifact(artifact: &Path) -> RunResult {
    let mut run_cmd = Command::new(artifact);
    let r = run_cmd.output().expect("Unable to run artifact.");
    RunResult {
        stdout: String::from_utf8(r.stdout).expect("Program created non-utf8 std out."),
    }
}

const TESTS: &[&str] = &[
    include_str!("./assignments/a5/test1.l"),
    // include_str!("./assignments/a5/test2.l"),
    include_str!("./assignments/a5/test3.l"),
    // include_str!("./assignments/a5/test4.l"),
    // include_str!("./assignments/a5/test5.l"),
    // include_str!("./assignments/a5/test6.l"),
];

#[test]
fn test_assignment_5_tests() {
    for (test, i) in TESTS.iter().zip(1..) {
        println!("Building test {}", i);
        let expected_result = read_expectations(test);

        let c_res = compile_program(test);
        if expected_result.compiles && c_res.is_err() {
            panic!(
                "Program fails to compile but should succeed. Compile errors: {:?}",
                c_res.err().unwrap()
            )
        }
        // else if !expected_result.compiles&& c_res.is_ok() {
        //     panic!("Program compiles but should fail to compile.");
        // }
        let Ok(program_bytes) = c_res else { continue };
        std::fs::write("out.s", program_bytes).expect("Failed to write program file.");

        link_runtime(Path::new("out.s"));

        let res = run_artifact(Path::new("./test_bin"));
        if let Some(e_stdout) = expected_result.stdout {
            assert_eq!(e_stdout, res.stdout);
        }
    }
}
