use std::collections::BinaryHeap;
use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();
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

    let visit = get_node_costs(&walls, start, end);
    let mut part1 = 0;
    let mut part2 = 0;

    for y in 0..visit.len() {
        for x in 0..visit[0].len() {
            for j in -20i32..21 {
                for i in -20i32..21 {
                    let (nx, ny) = ((x as i32 + i) as usize, (y as i32 + j) as usize);
                    let step = (i.abs() + j.abs()) as usize;
                    if step <= 20
                        && nx < visit[0].len()
                        && ny < visit.len()
                        && !walls[y][x]
                        && !walls[ny][nx]
                        && visit[y][x] >= visit[ny][nx] + 100 + step
                    {
                        if step == 2 {
                            part1 += 1;
                        }
                        part2 += 1
                    }
                }
            }
        }
    }

    println!("{:?}", part1);
    println!("{:?}", part2);
}

fn get_node_costs(
    walls: &Vec<Vec<bool>>,
    start: (usize, usize),
    end: (usize, usize),
) -> Vec<Vec<usize>> {
    let mut queue: BinaryHeap<(usize, (usize, usize))> = BinaryHeap::new();
    queue.push((1, (start.0, start.1)));
    let mut visit = vec![vec![usize::MAX; walls[0].len()]; walls.len()];
    visit[start.1][start.0] = 1;
    let mut best_cost = usize::MAX;

    while let Some((cost, (x, y))) = queue.pop() {
        let new_cost = cost + 1;
        for (nx, ny) in [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)] {
            if (nx, ny) == (end.0, end.1) {
                visit[ny][nx] = new_cost;
                if new_cost <= best_cost {
                    best_cost = new_cost;
                }
            } else if nx < walls[0].len()
                && ny < walls.len()
                && !walls[ny][nx]
                && new_cost <= visit[ny][nx]
            {
                visit[ny][nx] = new_cost;
                queue.push((new_cost, (nx, ny)));
            }
        }
    }

    for y in 0..walls.len() {
        for x in 0..walls[0].len() {
            if visit[y][x] != usize::MAX {
                visit[y][x] = best_cost - visit[y][x];
            }
        }
    }
    visit
}
