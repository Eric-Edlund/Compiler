// {"compiles": true, "stdout": "1\n1\n10\n"}

fn main() {
	let i = 1;
	let result = 1;
	print(i < 10);

	while i < 10 {
		i = i + 1;
		print(result);

		let j = 1;
		while j < 10 {
			result = result + 1;
			j = j + 1;
		}
	}

	print(result);
}
