// {"compiles": true, "stdout": ""}

fn main() {
	let i = 1;
	let factorial = 1;
	while i <= 5 {
		factorial = factorial * i;
		i = i + 1;
	}

	print(factorial);
}
