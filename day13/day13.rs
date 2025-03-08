use regex::Regex;
use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let re = Regex::new(r"([0-9]+)").unwrap();
    let results: Vec<i64> = re
        .find_iter(&contents)
        .filter_map(|digits| digits.as_str().parse::<i64>().ok())
        .collect();

    for off in [0, 10000000000000] {
        let tokens: u64 = results
            .chunks(6)
            .map(|x| match x[0..6] {
                [ax, ay, bx, by, gx, gy] => solve(ax, ay, bx, by, gx + off, gy + off),
                _ => 0,
            })
            .sum::<u64>();

        println!("{}", tokens);
    }
}

fn solve(ax: i64, ay: i64, bx: i64, by: i64, gx: i64, gy: i64) -> u64 {
    let a = (by * gx - bx * gy) / (ax * by - bx * ay);
    let b = (ax * gy - ay * gx) / (ax * by - bx * ay);

    if (by * gx - bx * gy) % (ax * by - bx * ay) == 0
        && (ax * gy - ay * gx) % (ax * by - bx * ay) == 0
    {
        (a * 3 + b) as u64
    } else {
        0
    }
}
