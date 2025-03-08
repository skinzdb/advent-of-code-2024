use regex::Regex;
use std::fs;

const GRID_WIDTH: u32 = 101;
const GRID_HEIGHT: u32 = 103;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let re = Regex::new(r"(-?[0-9]+)").unwrap();
    let results: Vec<i32> = re
        .find_iter(&contents)
        .filter_map(|digits| digits.as_str().parse::<i32>().ok())
        .collect();

    // Part 1
    let robots: Vec<(u32, u32)> = results
        .chunks(4)
        .filter_map(|x| match x[0..4] {
            [x, y, dx, dy] => Some(get_new_pos(x, y, dx, dy, 100)),
            _ => None,
        })
        .collect();

    let mut tl = 0;
    let mut tr = 0;
    let mut bl = 0;
    let mut br = 0;

    for (x, y) in robots {
        if x < GRID_WIDTH / 2 {
            if y < GRID_HEIGHT / 2 {
                tl += 1;
            } else if y > GRID_HEIGHT / 2 {
                bl += 1;
            }
        } else if x > GRID_WIDTH / 2 {
            if y < GRID_HEIGHT / 2 {
                tr += 1;
            } else if y > GRID_HEIGHT / 2 {
                br += 1;
            }
        }
    }

    println!("{}", tl * tr * bl * br);

    // Part2 (bad)
    for i in 1..10000 {
        let rs: Vec<(u32, u32)> = results
            .chunks(4)
            .filter_map(|x| match x[0..4] {
                [x, y, dx, dy] => Some(get_new_pos(x, y, dx, dy, i)),
                _ => None,
            })
            .collect();
        if test_tree(&rs) {
            println!("{}", i);
            print_tree(&rs);
        }
    }
}

fn get_new_pos(x: i32, y: i32, dx: i32, dy: i32, steps: u32) -> (u32, u32) {
    (
        ((x + steps as i32 * dx).rem_euclid(GRID_WIDTH as i32)) as u32,
        ((y + steps as i32 * dy).rem_euclid(GRID_HEIGHT as i32)) as u32,
    )
}

fn test_tree(robots: &Vec<(u32, u32)>) -> bool {
    for y0 in 0..GRID_HEIGHT {
        if robots.iter().filter(|(_, y1)| y0 == *y1).count() > 31 {
            return true;
        }
    }
    false
}

fn print_tree(robots: &Vec<(u32, u32)>) {
    for i in 0..GRID_HEIGHT {
        for j in 0..GRID_WIDTH {
            if robots.contains(&(j, i)) {
                print!("R");
            } else {
                print!("-");
            }
        }
        print!("\n");
    }
}
