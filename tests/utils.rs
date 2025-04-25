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
    const MSG: &str = "Must have // {..compile expectations..} on first line of test files";
    let json_src = &src_text.get(3..src_text.find('\n').expect(MSG)).expect(MSG);
    from_str::<TestExpectedResult>(json_src).unwrap()
}

#[derive(Debug)]
pub enum LinkResult {
    LinkerFailure,
    ChmodFailed,
}

pub fn link_runtime(prgm_assembly: &Path) -> Result<(), LinkResult> {
    let mut link_cmd = Command::new("gcc");
    link_cmd.args([
        "-g",
        "runtime.o",
        prgm_assembly.as_os_str().to_str().unwrap(),
        "-o",
        "test_bin",
    ]);
    let out = link_cmd.output().unwrap();
    if out.status.code().unwrap() != 0 {
        println!(
            "Failed to link \n\n{}",
            String::from_utf8(out.stderr).unwrap()
        );
        return Err(LinkResult::LinkerFailure);
    }

    let mut chmod = Command::new("chmod");
    chmod.args(["+x", "test_bin"]);
    match chmod.output() {
        Ok(..) => {}
        Err(..) => {
            println!("Failed to chmod +x the file");
            return Err(LinkResult::ChmodFailed);
        }
    }
    Ok(())
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
        println!("Testing program {}...", i);
        let expected_result = read_expectations(test);

        // Compile with all config combos
        let mut incorrect_failures: Vec<&CompilationConfig> = vec![];
        for config in COMPILATION_CONFIGS {
            let comp_res = compile_program(test, config);
            let Ok(ref text) = comp_res else {
                panic!("Failed to compile test program {}: {:?}", i, comp_res.err());
            };
            std::fs::write("out.s", text).expect("Failed to write program file.");
            let link_res = link_runtime(Path::new("out.s"));
            if (comp_res.is_err() || link_res.is_err()) && expected_result.compiles {
                incorrect_failures.push(config)
            }
        }

        if !incorrect_failures.is_empty() {
            println!();
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

        if !expected_result.compiles {
            continue;
        }

        let Ok(program_bytes) = c_res else {
            panic!("Failed to compile test program {}", i);
        };
        std::fs::write("out.s", program_bytes).expect("Failed to write program file.");

        let Ok(..) = link_runtime(Path::new("out.s")) else {
            panic!("Failed to liink runtime")
        };

        let res = run_artifact(Path::new("./test_bin"));
        if let Some(e_stdout) = expected_result.stdout {
            assert_eq!(e_stdout, res.stdout);
        }
    }
}
