use std::collections::HashSet;
use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let mut walls: HashSet<(usize, usize)> = HashSet::new();
    let mut boxes: HashSet<(usize, usize)> = HashSet::new();
    let mut moves: Vec<(i32, i32)> = Vec::new();
    let mut robot: (usize, usize) = (0, 0);

    for (j, line) in contents.lines().enumerate() {
        for (i, c) in line.chars().enumerate() {
            match c {
                '#' => {
                    walls.insert((i, j));
                }
                'O' => {
                    boxes.insert((i, j));
                }
                '@' => robot = (i, j),
                '^' => moves.push((0, -1)),
                '>' => moves.push((1, 0)),
                'v' => moves.push((0, 1)),
                '<' => moves.push((-1, 0)),
                _ => {}
            }
        }
    }

    for (dx, dy) in moves {
        let mut pos = (
            (robot.0 as i32 + dx) as usize,
            (robot.1 as i32 + dy) as usize,
        );

        let next_step = pos;

        while boxes.contains(&pos) {
            pos = ((pos.0 as i32 + dx) as usize, (pos.1 as i32 + dy) as usize);
        }

        if !walls.contains(&pos) {
            robot = next_step;
            if boxes.contains(&next_step) {
                boxes.remove(&next_step);
                boxes.insert(pos);
            }
        }
    }

    let gps_sum: usize = boxes.iter().map(|(x, y)| y * 100 + x).sum();

    println!("{}", gps_sum);

    part2();
}

fn part2() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let mut walls: HashSet<(usize, usize)> = HashSet::new();
    let mut boxes: HashSet<(usize, usize)> = HashSet::new();
    let mut moves: Vec<(i32, i32)> = Vec::new();
    let mut robot: (usize, usize) = (0, 0);

    for (j, line) in contents.lines().enumerate() {
        for (i, c) in line.chars().enumerate() {
            match c {
                '#' => {
                    walls.insert((i * 2, j));
                    walls.insert((i * 2 + 1, j));
                }
                'O' => {
                    boxes.insert((i * 2, j));
                }
                '@' => robot = (i * 2, j),
                '^' => moves.push((0, -1)),
                '>' => moves.push((1, 0)),
                'v' => moves.push((0, 1)),
                '<' => moves.push((-1, 0)),
                _ => {}
            }
        }
    }

    for (dx, dy) in moves {
        let next_step = (
            (robot.0 as i32 + dx) as usize,
            (robot.1 as i32 + dy) as usize,
        );
        let other_box_tile = (next_step.0 - 1, next_step.1);

        if dx == 0 {
            if !walls.contains(&next_step) {
                if !boxes.contains(&next_step) && !boxes.contains(&other_box_tile) {
                    robot = next_step;
                } else {
                    let box_pos = if boxes.contains(&next_step) {
                        next_step
                    } else {
                        other_box_tile
                    };
                    let touching = vert_connected(&boxes, box_pos.0, box_pos.1, dy);
                    if !touching.iter().any(|(x, y)| {
                        walls.contains(&(*x, (*y as i32 + dy) as usize))
                            || walls.contains(&(*x + 1, (*y as i32 + dy) as usize))
                    }) {
                        robot = next_step;
                        for (x, y) in touching {
                            boxes.remove(&(x, y));
                            boxes.insert((x, (y as i32 + dy) as usize));
                        }
                    }
                }
            }
        } else {
            let mut xs: Vec<i32> = vec![robot.0 as i32 + if dx == 1 { 1 } else { -2 }];
            let next_box = (xs[0] as usize, robot.1);
            let off = if dx == 1 { 0 } else { 1 };

            while boxes.contains(&(*xs.last().unwrap() as usize, robot.1)) {
                xs.push(*xs.last().unwrap() + dx * 2);
            }

            let last_x = *xs.last().unwrap();

            if !walls.contains(&next_step) {
                if !boxes.contains(&next_box) {
                    robot = next_step;
                }
                if !walls.contains(&((last_x + off) as usize, robot.1)) && boxes.contains(&next_box)
                {
                    xs.pop();
                    robot = next_step;
                    for x in xs {
                        boxes.remove(&(x as usize, robot.1));
                        boxes.insert(((x + dx) as usize, robot.1));
                    }
                }
            }
        }
    }

    let gps_sum: usize = boxes.iter().map(|(x, y)| y * 100 + x).sum();

    println!("{}", gps_sum);
}

fn vert_connected(
    boxes: &HashSet<(usize, usize)>,
    x: usize,
    y: usize,
    dy: i32,
) -> Vec<(usize, usize)> {
    if !boxes.contains(&(x, y)) {
        return vec![];
    }

    let mut a = vert_connected(boxes, x - 1, (y as i32 + dy) as usize, dy);
    let b = vert_connected(boxes, x, (y as i32 + dy) as usize, dy);
    let c = vert_connected(boxes, x + 1, (y as i32 + dy) as usize, dy);

    a.extend(&b);
    a.extend(&c);
    a.push((x, y));

    a
}
