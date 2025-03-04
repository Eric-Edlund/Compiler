// Design goals:
//
// - Mutation is a call site determination.
// - Monads without explicit lifting
// - Avoid temporary variable names

struct Option[T] {
	val: T
	is_err: bool
}

fn lift(Option[T]) =
	assert(!is_err) in
	self.val


fn under(Option[T], f: (T) -> R) {
	if not is_err:
		val.f
}

fn some()
	

fn factorial(n) = n * factorial(n-1)


// Comment
fn main() {
	let x = 8;
	x += 3 * 8 + 9;

	n | factorial | factorial -> x;

	x = input() // X is optional
	if Some(x):
		
	if (with x.lift) {
		print(x)
	} else {
		
	}

	if x == 0 {
		
	}

	print(x)
}

