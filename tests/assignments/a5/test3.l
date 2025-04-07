// {"compiles": true, "stdout": ""}

fn main() {
	let i = 1;
	let result = 1;
	while i < 10 {
		i = i + 1;
		if result < i {
			result = result + 1;
		} else {
			result = result - i;
		}
	}

	print(result);
}
