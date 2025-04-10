use compiler::{compile_program, CompilationConfig};
use serde::Deserialize;
use serde_json::from_str;
use std::path::Path;
use std::process::Command;

#[derive(Deserialize)]
pub struct TestExpectedResult {
    pub compiles: bool,
    pub stdout: Option<String>,
}

pub fn read_expectations(src_text: &str) -> TestExpectedResult {
    let json_src = &src_text[3..src_text.find('\n').unwrap()];
    from_str::<TestExpectedResult>(json_src).unwrap()
}

pub fn link_runtime(prgm_assembly: &Path) {
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

pub struct RunResult {
    pub stdout: String,
}

pub fn run_artifact(artifact: &Path) -> RunResult {
    let mut run_cmd = Command::new(artifact);
    let r = run_cmd.output().expect("Unable to run artifact.");
    RunResult {
        stdout: String::from_utf8(r.stdout).expect("Program created non-utf8 std out."),
    }
}

pub const COMPILATION_CONFIGS: &[CompilationConfig] = &[
    CompilationConfig {
        no_registers: false,
    },
    CompilationConfig { no_registers: true },
];

pub fn test_files(tests: &[&str]) {
    for (test, i) in tests.iter().zip(1..) {
        println!("Testing program {}", i);
        let expected_result = read_expectations(test);

        // Compile with all config combos
        let mut incorrect_failures: Vec<&CompilationConfig> = vec![];
        for config in COMPILATION_CONFIGS {
            let res = compile_program(test, config);
            if res.is_err() && expected_result.compiles {
                incorrect_failures.push(config)
            }
        }

        if !incorrect_failures.is_empty() {
            println!(
                "Failed to compile under the following configurations: {:?}",
                incorrect_failures
            );
            panic!();
        }

        let c_res = compile_program(test, &COMPILATION_CONFIGS[0]);
        if expected_result.compiles && c_res.is_err() {
            panic!(
                "Program fails to compile but should succeed. Compile errors: {:?}",
                c_res.err().unwrap()
            )
        }

        let Ok(program_bytes) = c_res else { continue };
        std::fs::write("out.s", program_bytes).expect("Failed to write program file.");

        link_runtime(Path::new("out.s"));

        let res = run_artifact(Path::new("./test_bin"));
        if let Some(e_stdout) = expected_result.stdout {
            assert_eq!(e_stdout, res.stdout);
        }
    }
}
