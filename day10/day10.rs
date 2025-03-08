use std::fs;

const SIZE: usize = 41;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let grid: Vec<Vec<u32>> = contents
        .lines()
        .map(|row| row.chars().map(|x| x.to_digit(10).unwrap()).collect())
        .collect();

    let mut sum = 0;
    let mut sum2 = 0;

    for row in 0..SIZE {
        for col in 0..SIZE {
            if grid[row][col] == 0 {
                let mut visit = [[false; SIZE]; SIZE];
                sum2 += search_trail(&grid, &mut visit, row as u32, col as u32, 0, false);
                sum += search_trail(&grid, &mut visit, row as u32, col as u32, 0, true);
            }
        }
    }

    println!("{}", sum);
    println!("{}", sum2);
}

fn search_trail(
    grid: &Vec<Vec<u32>>,
    visit: &mut [[bool; SIZE]; SIZE],
    row: u32,
    col: u32,
    prev: u32,
    part1: bool
) -> u32 {
    if row < SIZE as u32
        && col < SIZE as u32
        && !visit[row as usize][col as usize]
        && grid[row as usize][col as usize] == prev
    {
        if prev == 9 {
            visit[row as usize][col as usize] = part1;
            return 1;
        }

        search_trail(grid, visit, row - 1, col, prev + 1, part1)
            + search_trail(grid, visit, row + 1, col, prev + 1, part1)
            + search_trail(grid, visit, row, col - 1, prev + 1, part1)
            + search_trail(grid, visit, row, col + 1, prev + 1, part1)
    } else {
        0
    }
}
