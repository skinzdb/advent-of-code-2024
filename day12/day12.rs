use std::collections::HashSet;
use std::fs;

fn main() {
    let contents = fs::read_to_string("test-data.txt").unwrap();

    let grid: Vec<Vec<char>> = contents.lines().map(|row| row.chars().collect()).collect();
    let mut sum = 0;
    let mut visit = HashSet::new();

    for r in 0..grid.len() {
        for c in 0..grid[0].len() {
            let (shape, perim) = fill(&grid, &mut visit, r as u32, c as u32, grid[r][c]);
            sum += shape.len() as u32 * perim;

            if !shape.is_empty() {
                println!("{}", get_sides(&shape));
            }
        }
    }

    println!("{}", sum);
}

fn fill(
    grid: &Vec<Vec<char>>,
    visit: &mut HashSet<(u32, u32)>,
    row: u32,
    col: u32,
    prev: char,
) -> (Vec<(u32, u32)>, u32) {
    if visit.contains(&(row, col)) {
        if grid[row as usize][col as usize] == prev {
            return (vec![], 0);
        } else {
            return (vec![], 1);
        }
    }

    if col >= grid[0].len() as u32
        || row >= grid.len() as u32
        || grid[row as usize][col as usize] != prev
    {
        return (vec![], 1);
    }

    visit.insert((row, col));

    let mut neighbours: (Vec<Vec<(u32, u32)>>, Vec<u32>) = [
        (row + 1, col),
        (row, col + 1),
        (row - 1, col),
        (row, col - 1),
    ]
    .iter()
    .map(|(r, c)| fill(grid, visit, *r, *c, prev))
    .unzip();

    neighbours.0.push(vec![(row, col)]);

    (neighbours.0.concat(), neighbours.1.iter().sum::<u32>())
}

fn get_sides(shape: &Vec<(u32, u32)>) -> usize {
    let mut visit: HashSet<(u32, u32)> = HashSet::new();
    let mut sides = 0;

    for (row, col) in shape {
        sides += match [
            (*row + 1, *col),
            (*row, *col + 1),
            (*row - 1, *col),
            (*row, *col - 1),
        ]
        .iter()
        .map(|x| {
            visit.insert(*x);
            shape.contains(x)
        })
        .collect::<Vec<bool>>()[0..4]
        {
            [false, false, false, false] => 4,
            [true, false, false, false] => {
                if shape.contains(&(*row + 2, *col)) {
                    2
                } else {
                    3
                }
            }
            [false, true, false, false] => {
                if shape.contains(&(*row, *col + 2)) {
                    2
                } else {
                    3
                }
            }
            [false, false, true, false] => {
                if shape.contains(&(*row - 2, *col)) {
                    2
                } else {
                    3
                }
            }
            [false, false, false, true] => {
                if shape.contains(&(*row, *col - 2)) {
                    2
                } else {
                    3
                }
            }

            [true, true, false, false] => 1,
            [false, true, true, false] => 1,
            [false, false, true, true] => 1,
            [true, false, false, true] => 1,
            [true, _, true, _] => 0,
            [_, true, _, true] => 0,
            _ => 0,
        };
    }

    sides
    // 8 neighbours
    // 0 perims => 4 sides
    // 2 straight perims => 0 sides
    // 1 straight perim => 1 side
    // 1 diagonal perim => 2 sides
    // 2 diagonal perims => 1 side

    // for (r, c) in perims {
    //     let dirs = [
    //         (*r + 1, *c),
    //         (*r + 1, *c + 1),
    //         (*r, *c + 1),
    //         (*r - 1, *c + 1),
    //         (*r - 1, *c),
    //         (*r - 1, *c - 1),
    //         (*r, *c - 1),
    //         (*r + 1, *c - 1),
    //     ];

    //     let cum = match dirs.iter().map(|x| perims.contains(x)).collect::<Vec<bool>>()[0..8] {
    //         [false, _, false, _, false, _, false, _] => 4,
    //         [true, _, _, _, true, _, _, _] => 0,
    //         [_, _, true, _, _, _, true, _] => 0,
    //         [_, true, _, ]
    //         []
    //         _ => 0
    //     };

    //     let shit: Vec<&(u32, u32)> = dirs.iter().filter(|x| perims.contains(*x)).collect();

    //     let mut new_sides = 4 - shit.len();

    //     if shit.iter().any(|x| visit.contains(*x)) {
    //         new_sides = 0;
    //     }

    //     sides += new_sides;

    //     visit.insert((*r, *c));
    // }
}
