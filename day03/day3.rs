use regex::Regex;
use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    // Part 1
    let re = Regex::new(r"mul\(([0-9]+),([0-9]+)\)").unwrap();
    let results: i32 = re
        .captures_iter(&contents)
        .map(|c| c.extract())
        .map(|(_, [a, b])| a.parse::<i32>().unwrap() * b.parse::<i32>().unwrap())
        .sum();
    println!("{:?}", results);

    // Part 2
    let re2 = Regex::new(r"(do\(\)())|(don't\(\)())|mul\(([0-9]+),([0-9]+)\)").unwrap();
    let results2: i32 = re2
        .captures_iter(&contents)
        .map(|c| c.extract())
        .fold((1, 0), |(on, total), x| match x.1 {
            ["do()", _] => (1, total),
            ["don't()", _] => (0, total),
            [a, b] => (
                on,
                on * a.parse::<i32>().unwrap() * b.parse::<i32>().unwrap() + total,
            ),
        })
        .1;
    println!("{:?}", results2);
}
