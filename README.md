# Very Cool Compiler

Bizzare Unsolved Problems:

- Assignment 7 doesn't work half the time when registers are enabled. I am saving
  and restoring registers at the callee/caller sites but still there's some unexpected
  behavior sometimes. The sometimes is because register assignment is not
  deterministic, hashing, I havn't figured out what's happening, the generated
  code looks right?

- Assignment 6: There is some problem with deeply nested tuples that I'm not
  understanding. Again, the generated code looks correct but I guess I'm missing
  something.

Expected Stuff that works:

- Assignments 4, 5 compile and behave as expected.
- Assignment 7 is correct when compiling with the --no-registers flag. The integration
  tests do this by default.
- Assignment 6 works for half the tests that don't involve deeply nested tuples.

Extra Stuff that works:

- Integration tests build and run the test programs and compare std out
- Did all the parsing by hand, very confusing indeed
- Rust
- Option to compile without using registers


```bash
# With this command, the tests give up after the first file fails to compile/run
# with correct output. The generated assembly is in out.s
cargo test

# These build the test file and run it on the host machine,
# comparing the stdout to the expected value.
cargo test --test assignment_4
cargo test --test assignment_5
cargo test --test assignment_6
cargo test --test assignment_7

# Invoke the compiler on a source file.
cargo run -- tests/assignments/a7/test5.l -d -o out.s --no-registers &&
  gcc out.s runtime.o -o a.out -g &&
  ./a.out
```
