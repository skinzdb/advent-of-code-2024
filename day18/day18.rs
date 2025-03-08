use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::fs;

const GRID_SIZE: usize = 71;

#[derive(PartialEq, Eq)]
struct Point {
    x: usize,
    y: usize,
    cost: u32,
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let mut barriers = [[false; GRID_SIZE]; GRID_SIZE];

    let coords: Vec<(usize, usize)> = contents
        .lines()
        .map(|l| match l.split(",").collect::<Vec<&str>>()[0..2] {
            [x, y] => (x.parse::<usize>().unwrap(), y.parse::<usize>().unwrap()),
            _ => (0, 0),
        })
        .collect();

    for &(x, y) in coords[..1024].iter() {
        barriers[y][x] = true;
    }

    println!("{:?}", best_cost(&barriers));

    let mut block_idx = 1024;
    while let Some(_) = best_cost(&barriers) {
        let (x, y) = coords[block_idx];
        barriers[y][x] = true;
        block_idx += 1;
    }

    println!("{:?}", coords[block_idx-1]);
}

fn best_cost (barriers: &[[bool; GRID_SIZE]; GRID_SIZE]) -> Option<u32> {
    let mut queue: BinaryHeap<Point> = BinaryHeap::new();
    queue.push(Point {
        x: 0,
        y: 0,
        cost: 1,
    });
    let mut visit = [[false; GRID_SIZE]; GRID_SIZE];
    visit[0][0] = true;

    while let Some(Point { x, y, cost }) = queue.pop() {
        for (nx, ny) in [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)] {
            if (nx, ny) == (GRID_SIZE - 1, GRID_SIZE - 1) {
                return Some(cost);
            } else if nx < GRID_SIZE && ny < GRID_SIZE && !barriers[ny][nx] && !visit[ny][nx] {
                visit[ny][nx] = true;
                queue.push(Point {
                    x: nx,
                    y: ny,
                    cost: cost + 1,
                });
            }
        }
    }
    None
}
