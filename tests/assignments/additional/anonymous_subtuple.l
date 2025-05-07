// {"compiles": true, "stdout": "42\n"}

fn main() {
	let x = (42, (1, 2));

	print(x[1][0]);
}
