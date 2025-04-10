// {"compiles": true, "stdout": "100\n100\n100\n100\n100\n100\n100\n100\n100\n10\n"}

fn main() {
	let i = 1;
	let result = 1;
	while i < 10 {
		i = i + 1;
		if result < i {
			print(100);
			result = result + 1;
		} else {
			print(200);
			result = result - i;
		}
	}

	print(result);
}
