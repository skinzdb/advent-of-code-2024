use std::fs;

fn main() {
    let contents = fs::read_to_string("data.txt").unwrap();

    let rows: Vec<&str> = contents.lines().collect();

    let mut pos: Vec<(usize, usize)> = rows
        .iter()
        .enumerate()
        .flat_map(|(row, &p)| match p.chars().position(|c| c == '^') {
            Some(col) => Some((col, row)),
            None => None,
        })
        .collect();

    let mut dir: (i32, i32) = (0, -1);

    while dir != (0, 0) {
        let (x, y) = pos.last().unwrap();

        dir = match rows.get((*y as i32 + dir.1) as usize) {
            None => (0, 0),
            Some(r) => match r.chars().nth((*x as i32 + dir.0) as usize) {
                None => (0, 0),
                Some('#') => rotate(dir),
                Some(_) => dir,
            },
        };

        pos.push(((*x as i32 + dir.0) as usize, (*y as i32 + dir.1) as usize));
    }

    pos.sort();
    pos.dedup();



    //place an obstacle s.t. it takes you to a previously seen obstacle
    //
    /*       #   #
        #          #
                     #
       #
             #
       #
                   #
        ^
        make squares, check it is on original path
        to check a square: four case: bottom left corner, bottom right, top left, top right
    */



    println!("{}", pos.len());
}

fn check_loop (grid: &Vec<&str>, pos: (i32, i32)) -> bool {
    let left = grid.get((pos.1 - 1) as usize);
    gghgg






    /*      t             t
                          r
                        l             r
                                    l
           t
                         #
                                b   lr

                        b
                               b

    */

}

fn rotate(dir: (i32, i32)) -> (i32, i32) {
    match dir {
        (0, -1) => (1, 0),
        (1, 0) => (0, 1),
        (0, 1) => (-1, 0),
        (-1, 0) => (0, -1),
        _ => (0, 0),
    }
}
