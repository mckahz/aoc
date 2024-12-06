use nalgebra::*;

type Board = Matrix5<u8>;

#[derive(Debug)]
struct BingoSubsystem {
    boards: Vec<Board>,
    draws: Vec<u8>,
}

fn parse(input: String) -> BingoSubsystem {
    let mut chunks = input.split("\n\n");
    let draws = chunks
        .next()
        .unwrap()
        .split(",")
        .filter_map(|n| n.parse().ok())
        .collect();

    let boards = chunks
        .map(|chunk| {
            Matrix5::from_vec(
                chunk
                    .lines()
                    .flat_map(|line| {
                        line.split(" ")
                            .filter_map(|n| n.trim().parse().ok())
                            .collect::<Vec<u8>>()
                    })
                    .collect(),
            )
            .transpose()
        })
        .collect();

    BingoSubsystem { boards, draws }
}

fn has_won(board: &Matrix5<u8>, draws: &[u8]) -> bool {
    let winning_row = || {
        board
            .row_iter()
            .any(|row| row.iter().all(|num| draws.contains(num)))
    };
    let winning_col = || {
        board
            .column_iter()
            .any(|col| col.iter().all(|num| draws.contains(num)))
    };
    winning_row() || winning_col()
}

fn score(board: &Matrix5<u8>, draws: &[u8]) -> u32 {
    let sum: u32 = board
        .iter()
        .filter(|num| !draws.contains(num))
        .map(|num| *num as u32)
        .sum();
    let last_draw = *draws.iter().last().unwrap() as u32;
    return last_draw * sum;
}

pub fn part1(input: String) -> u32 {
    let bingo_subsystem = parse(input);
    let mut draws = vec![];
    for draw in bingo_subsystem.draws {
        draws.push(draw);
        let winning_board = bingo_subsystem
            .boards
            .iter()
            .find(|board| has_won(*board, &draws));
        match winning_board {
            Some(board) => return score(board, &draws),
            None => continue,
        }
    }
    panic!("No winning boards found");
}

fn final_score<'a>(boards: &'a [Board], draws: &'a [u8]) -> Option<(&'a Board, &'a [u8])> {
    let previous_draws = &draws[..draws.len() - 1];
    match boards.iter().find(|board| !has_won(board, previous_draws)) {
        Some(board) => {
            Some((board, draws))
        },
        None => final_score(boards, previous_draws),
    }
}

pub fn part2(input: String) -> u32 {
    let bingo_subsystem = parse(input);
    let (winning_board, draws) = final_score(&bingo_subsystem.boards, &bingo_subsystem.draws).expect("no boards won ever?");
    score(&winning_board, &draws)
}
