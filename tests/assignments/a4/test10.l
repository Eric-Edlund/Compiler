// {"compiles": true}


fn main() {
    let x = 5;
    let y = 7;
    let z = 0;

    if 5 == 6 {
        if x == y {
            z = x;
        } else {
            z = x + y;
        }
    } else {
        if x == y {
            z = x + y;
        } else {
            z = 1;
        }
    }

    print(z)
}
