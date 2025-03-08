use regex::Regex;
use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let re = Regex::new(r"([0-9]+)").unwrap();
    let results: Vec<u64> = re
        .find_iter(&contents)
        .filter_map(|digits| digits.as_str().parse::<u64>().ok())
        .collect();

    let mut a: u64 = results[0];
    let mut b: u64 = results[1];
    let mut c: u64 = results[2];
    let mut ins_ptr = 0;
    let mut output: Vec<u64> = Vec::new();

    let program = &results[3..];

    while ins_ptr < program.len() {
        let combo = [0, 1, 2, 3, a, b, c];
        match program[ins_ptr..ins_ptr + 2] {
            [0, val] => a >>= combo[val as usize],
            [1, val] => {
                b ^= val;
            }
            [2, val] => {
                b = combo[val as usize] & 0b111;
            }
            [3, val] => {
                if a != 0 {
                    ins_ptr = val as usize - 2;
                }
            }
            [4, _] => {
                b ^= c;
            }
            [5, val] => {
                output.push(combo[val as usize] & 0b111);
            }
            [6, val] => b = a >> combo[val as usize],
            [7, val] => c = a >> combo[val as usize],
            _ => break,
        }
        ins_ptr += 2;
    }
    println!("{:?}", output);

    println!("{:?}", part2(&program, 0));
}

fn part2(program: &[u64], prev: u64) -> Option<u64> {
    if program.is_empty() {
        return Some(prev);
    }
    // Only care about the last 10 bits of `a` at a time;
    // when calculating `out`, we right shift `a` by at most 7
    // and then take the last 3 bits, so 10 in total
    for a in 1..1 << 10 {
        // Calculate output fast according to my specific input instructions
        let out = ((((a & 7) ^ 3) ^ (a >> ((a & 7) ^ 3))) ^ 3) & 7;

        // Output must match input AND `a` >> 3 has to equal last 7 bits of `prev`
        if Some(&out) == program.last() && a >> 3 == prev & 127 {
            if let Some(solution) = part2(&program[..program.len() - 1], prev << 3 | a & 7) {
                return Some(solution);
            }
        }
    }
    None
}
