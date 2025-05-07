// {"compiles": true, "stdout": "10\n"}

fn main() {
	let x = (1, 10);
	let y = (42, (x, 2));

	print(y[1][0][1]);
}
