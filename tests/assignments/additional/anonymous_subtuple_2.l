// {"compiles": true, "stdout": "42\n2\n10\n"}

fn main() {
	let x = (1, 10);
	let y = (42, (x, 2));
	let z = (42, (x, 2));
	print(x);
	print(y);
	print(z);

	print(y[1][0]);
	print(y[1][0][0]);
}
