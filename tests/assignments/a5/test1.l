// {"compiles": true, "stdout": "10\n"}

fn main() {
	let i = 1;
	let result = 1;
	while i < 10 {
		result = result + 1;
		i = i + 1;
	}
	print(result);
}
