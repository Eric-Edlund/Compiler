// {"compiles": true, "stdout": "120\n"}

fn fact(n: int) -> int {
    if n == 0 {
        return 1;
    } else {
        return n * fact(n-1);
    }
}

fn main() {
    print(fact(5));
}
