// {"compiles": true, "stdout": "7\n"}
// z not defined in outer scope

fn main() {
    let x = 5;
    let y = 7;
    let z = 0;

    if 5 == 6 {
        z = x;
    } else {
        z = y;
    }

    print(z)
}
