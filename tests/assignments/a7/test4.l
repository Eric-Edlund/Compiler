// {"compiles": true, "stdout": "6\n7\n"}

fn add1_and2(n: int) -> (int, int) {
    return (n + 1, n + 2);
}

fn main() {
    let z = add1_and2(5);
    print(z[0]);
    print(z[1]);
}
