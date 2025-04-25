// {"compiles": true, "stdout": "1\n"}

fn main() {
	let x = (1, 2);
	let y = (3, x);
	print(y[1][0]);
}
