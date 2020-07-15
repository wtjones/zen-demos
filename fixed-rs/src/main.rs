struct Fixed8Dot8 {
    whole: i8,
    frac: i8,
}

fn main() {
    println!("Enter input for 8.8");
    println!("number:");

    let mut float = String::new();

    std::io::stdin()
        .read_line(&mut float)
        .expect("Failed to read line");

    let float: f64 = float.trim().parse().expect("bad!");

    let fixed_whole = float.trunc() as i32;
    let fixed_frac = (float.fract() * 256_f64) as i32;
    let fixed_8_8 = fixed_whole * 256 + fixed_frac;

    println!(
        "whole: {}, frac: {}, fixed: {:#016b}",
        fixed_whole, fixed_frac, fixed_8_8
    );
}
