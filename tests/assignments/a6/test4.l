// {"compiles": true, "stdout": "42\n"}

fn main() {
	let x = (42, 2);
	let y = (3, (x, x));
	let z = (y, x);
	print(z[0][1][0][0]);

}
