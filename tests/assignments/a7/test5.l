// {"compiles": true, "stdout": "55\n"}

fn fib(n: int) -> int {
    if n <= 1 {
	return n;
    } else {
	return fib(n-1) + fib(n-2);
    }
}

fn main() {
    print(fib(10));
}
