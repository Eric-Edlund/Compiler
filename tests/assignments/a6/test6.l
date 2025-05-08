// {"compiles": true, "stdout": "1\n"}

fn main() {
	let i = 100;
	let x = (0, 1); // Parser doesn't support (0,) like in python
	while i > 0 {;
		i = i - 1;
		let x = (1,2,3);
	}
	print(x[0]);
}
