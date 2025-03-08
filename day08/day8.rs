use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let mut tower_map = HashMap::new();

    for (y, r) in contents.lines().enumerate() {
        for (x, c) in r.chars().enumerate() {
            if c != '.' {
                tower_map
                    .entry(c)
                    .or_insert(Vec::new())
                    .push((x as i32, y as i32));
            }
        }
    }

    let max_x = contents.find('\n').unwrap() as i32;
    let max_y = contents.lines().count() as i32;
    let mut antinodes1 = HashSet::new();
    let mut antinodes2 = HashSet::new();

    for (_, v) in tower_map {
        for node in get_antinodes(&v, (max_x, max_y), true) {
            antinodes1.insert(node);
        }
        for node in get_antinodes(&v, (max_x, max_y), false) {
            antinodes2.insert(node);
        }
    }

    println!("{}", antinodes1.len());
    println!("{}", antinodes2.len());
}

fn get_antinodes(
    nodes: &Vec<(i32, i32)>,
    (max_x, max_y): (i32, i32),
    part1: bool,
) -> Vec<(i32, i32)> {
    let mut antinodes = Vec::new();

    for i in 0..nodes.len() - 1 {
        for j in i + 1..nodes.len() {
            let mut pos1 = nodes[i];
            let mut pos2 = nodes[j];

            let dx = pos2.0 - pos1.0;
            let dy = pos2.1 - pos1.1;

            pos1 = (pos1.0 - dx, pos1.1 - dy);
            pos2 = (
                pos2.0 + if part1 { dx } else { -dx },
                pos2.1 + if part1 { dy } else { -dy },
            );

            while pos1.0 >= 0 && pos1.1 >= 0 && pos1.0 < max_x && pos1.1 < max_y {
                antinodes.push(pos1);
                if part1 {
                    break;
                }
                pos1 = (pos1.0 - dx, pos1.1 - dy);
            }

            while pos2.0 >= 0 && pos2.1 >= 0 && pos2.0 < max_x && pos2.1 < max_y {
                antinodes.push(pos2);
                if part1 {
                    break;
                }
                pos2 = (pos2.0 + dx, pos2.1 + dy);
            }
        }
    }

    antinodes
}
