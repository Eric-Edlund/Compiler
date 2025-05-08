Very Cool Compiler

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

f at most 1 page, describing the approach you took in your implementation,
and features you planned but did not

```bash
cargo test

cargo run -- tests/assignments/a7/test5.l -d -o out.s --no-registers && gcc out.s runtime.o -o a.out -g && ./a.out
```
