// {"compiles": false}
// z not defined in outer scope

fn main() {
    let x = 5;
    let y = 7;

    if 5 == 6 {
        z = x;
    } else {
        z = y;
    }

    print(z)
}
