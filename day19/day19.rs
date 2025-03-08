use std::collections::HashMap;
use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();
    let lines: Vec<&str> = contents.lines().collect();

    let blocks: Vec<&str> = lines[0].split(", ").collect();
    let targets = &lines[2..];

    let p1: usize = targets.iter().filter(|x| part1(&blocks, x)).count();

    let p2: usize = targets
        .iter()
        .map(|x| part2(&blocks, x, &mut HashMap::new()))
        .sum();

    println!("{}", p1);
    println!("{}", p2);
}

fn part1(blocks: &Vec<&str>, target: &str) -> bool {
    if target.is_empty() {
        return true;
    }

    blocks.iter().any(|b| {
        b.len() <= target.len() && **b == target[..b.len()] && part1(blocks, &target[b.len()..])
    })
}

fn part2<'a>(blocks: &Vec<&str>, target: &'a str, seen: &mut HashMap<&'a str, usize>) -> usize {
    if target.is_empty() {
        return 1;
    }

    blocks
        .iter()
        .map(|b| {
            if b.len() <= target.len() && **b == target[..b.len()] {
                let new_target = &target[b.len()..];
                match seen.get(new_target) {
                    None => {
                        let val = part2(blocks, new_target, seen);
                        seen.insert(new_target, val);
                        val
                    }
                    Some(&x) => x,
                }
            } else {
                0
            }
        })
        .sum()
}
