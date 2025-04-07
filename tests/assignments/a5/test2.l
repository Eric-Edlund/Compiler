// {"compiles": true, "stdout": ""}

fn main() {
	let i = 1;
	let result = 1;
	while i < 10 {
		i = i + 1;

		let j = 1;
		while j < 10 {
			result = result + 1;
			j = j + 1;
		}
	}

	print(result);

}
