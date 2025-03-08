use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let expanded: Vec<i32> = contents
        .chars()
        .enumerate()
        .map(|(i, c)| {
            let num = c.to_digit(10).unwrap() as i32;
            if i & 1 == 0 {
                num
            } else {
                -num
            }
        })
        .collect();

    let files: Vec<&i32> = expanded.iter().rev().filter(|&&x| x > 0).collect();

    for f in files {
        for i in expanded {

        }
    }
}
