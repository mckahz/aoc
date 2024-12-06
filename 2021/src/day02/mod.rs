enum Dir {
    U,
    D,
    F,
}
struct Command {
    dir: Dir,
    amount: i32,
}

fn parse(input: String) -> Vec<Command> {
    input
        .lines()
        .map(|line| {
            let mut l = line.split(" ").into_iter();
            let dir = l.next().unwrap();
            let dir = if dir.starts_with("u") {
                Dir::U
            } else if dir.starts_with("d") {
                Dir::D
            } else if dir.starts_with("f") {
                Dir::F
            } else {
                panic!("invalid direction")
            };
            let amount = l.next().unwrap().parse().unwrap();
            Command { dir, amount }
        })
        .collect()
}

pub fn part1(input: String) -> u32 {
    let commands = parse(input);
    let mut dist = 0;
    let mut depth = 0;
    for command in commands {
        match command.dir {
            Dir::U => depth -= command.amount,
            Dir::D => depth += command.amount,
            Dir::F => dist += command.amount,
        }
    }
    (depth * dist) as u32
}

pub fn part2(input: String) -> u32 {
    let commands = parse(input);
    let mut dist = 0;
    let mut depth = 0;
    let mut aim = 0;
    for command in commands {
        match command.dir {
            Dir::U => aim -= command.amount,
            Dir::D => aim += command.amount,
            Dir::F => { dist += command.amount; depth += aim * command.amount },
        }
    }
    (depth * dist) as u32
}
