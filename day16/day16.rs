use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashSet;
use std::fs;
use std::usize;

#[derive(PartialEq, Eq, Clone)]
struct Point {
    pos: (usize, usize),
    dir: usize,
    prev: Option<Box<Point>>,
    cost: usize,
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.pos.cmp(&other.pos))
            .then_with(|| self.dir.cmp(&other.dir))
            .then_with(|| self.prev.cmp(&other.prev))
    }
}

fn main() {
    let contents = fs::read_to_string("test-data.txt").unwrap();

    let (cols, rows) = (
        contents.lines().count(),
        contents.lines().collect::<Vec<&str>>()[0].len(),
    );

    let mut walls: Vec<Vec<bool>> = vec![vec![false; rows]; cols];
    let mut start = (0, 0);
    let mut end = (0, 0);

    for (y, l) in contents.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            match c {
                '#' => walls[y][x] = true,
                'S' => start = (x, y),
                'E' => end = (x, y),
                _ => {}
            }
        }
    }

    let (nodes, cost) = best_paths(&walls, start, end);

    for y in 0..walls.len() {
        for x in 0..walls[0].len() {
            if walls[y][x] {
                print!("#");
            } else if nodes.contains(&(x, y)) {
                print!("O");
            } else {
                print!(".");
            }
        }
        print!("\n");
    }

    println!("{}", cost);
    println!("{}", nodes.len());
}

fn best_paths(
    walls: &Vec<Vec<bool>>,
    start: (usize, usize),
    end: (usize, usize),
) -> (HashSet<(usize, usize)>, usize) {
    let mut queue: BinaryHeap<Point> = BinaryHeap::new();
    queue.push(Point {
        pos: start,
        dir: 0,
        prev: None,
        cost: 1,
    });
    let mut visit = vec![vec![usize::MAX; walls[0].len()]; walls.len()];
    visit[start.1][start.0] = 1;

    let mut best_nodes = HashSet::new();
    let mut best_cost: usize = usize::MAX;

    while let Some(p) = queue.pop().as_ref() {
        let Point { pos, dir, prev: _, cost, } = p;
        for (d, (nx, ny)) in [
            (0, (pos.0 + 1, pos.1)),
            (1, (pos.0, pos.1 + 1)),
            (2, (pos.0 - 1, pos.1)),
            (3, (pos.0, pos.1 - 1)),
        ] {
            let dir_penalty = if d != *dir { 1000 } else { 0 };
            let new_cost = cost + dir_penalty + 1;
            if (nx, ny) == end {
                if new_cost <= best_cost {
                    best_cost = new_cost;
                    for node in get_path(&p) {
                        best_nodes.insert(node);
                    }
                }
            } else if nx < walls[0].len()
                && ny < walls.len()
                && !walls[ny][nx]
                && new_cost <= visit[ny][nx]
            {
                visit[ny][nx] = new_cost;
                queue.push(Point {
                    pos: (nx, ny),
                    dir: d,
                    prev: Some(Box::new(p.clone())),
                    cost: new_cost,
                });
            }
        }
    }
    best_nodes.insert(end);
    (best_nodes, best_cost)
}

fn get_path(point: &Point) -> Vec<(usize, usize)> {
    let mut best_nodes = vec![point.pos];
    let mut curr = &point.prev;

    while let Some(p) = curr.as_ref() {
        best_nodes.push(p.pos);
        curr = &p.prev;
    }

    best_nodes
}
