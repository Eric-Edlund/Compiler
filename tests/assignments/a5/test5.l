// {"compiles": true, "stdout": "120\n"}

fn main() {
	let x = 5;
	let factorial = 0;
	if x < 10 {
		let i = 1;
		factorial = 1;
		while i <= 5 {
			factorial = factorial * i;
			i = i + 1;
		}
	} else {
		factorial = 200;
	}

	print(factorial);
}
