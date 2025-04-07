// {"compiles": true, "stdout": "0\n9\n"}

fn main() {
    // x = false
    let x = ((5 == 6) and ((6+7) > 3) or (5 < 3));

    print(x);

    if !x {
        print(9);
    } else {
        print(10);
    }
}
