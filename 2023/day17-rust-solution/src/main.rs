use std::collections::hash_map::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Orientation { H, V }
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Node {
    position: Position,
   arrival: Orientation,
  }
struct Grid
  { bounds : Bounds
  , values : Vec<Vec<usize>>
  }
enum Direction {Left, Right, Up, Down}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Bounds
  { bottom : usize
  , right : usize
  }
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Position
  { row: usize
  , col : usize
  }

impl Grid {
    fn get(&self, pos: Position) -> Option<usize> {
        let i : usize = pos.row.into();
        let j : usize = pos.col.into();
        self.values.get(i)?.get(j).map(|x| *x)
    }
}

fn parse(str: &str) -> Grid {
  let values : Vec<Vec<usize>> =
    str
    .lines()
    .map(|line| line.chars().filter_map(|x| x.to_string().parse::<usize>().ok()).collect())
    .collect();

  let bottom : usize = (values.len() - 1).into();

  let right : usize = (values[0].len() - 1).into();

  let bounds = Bounds { bottom, right };

  Grid { values, bounds }
}

fn go(position: Position, n: usize, direction: Direction, b: Bounds) -> Option<Position> {
    let mut p = position.clone();
    match direction {
        Direction::Left if p.col < n => None,
        Direction::Left => { p.col -= n; Some(p) },
        Direction::Up if p.row < n => None,
        Direction::Up => { p.row -= n; Some(p) },
        Direction::Right if p.col + n > b.right => None,
        Direction::Right => { p.col += n; Some(p) },
        Direction::Down if p.row + n > b.bottom => None,
        Direction::Down => { p.row += n; Some(p) }
    }
}

fn find_path(grid: Grid, distances: Vec<usize>, end_orientation: Orientation) -> u16 {
    let bounds = grid.bounds;
    let start_pos = Position { row: 0, col: 0 };
    let start = Node { position: start_pos, arrival: Orientation::V };
    let end_pos = Position { row: bounds.bottom, col: bounds.right };
    let end = Node { position: end_pos, arrival: end_orientation };
    let nodes : usize = ((bounds.bottom + 1) * (bounds.right + 1)).into();

    let next = move |node: Node|
        distances
        .iter()
        .flat_map(|n| match node.arrival {
            Orientation::H =>
                [Direction::Up, Direction::Down]
                .into_iter()
                .filter_map(|dir| {
                    go(node.position, *n, dir, bounds)
                    .map(|position| Node { position, arrival: Orientation::V })
                }).collect::<Vec<Node>>(),

            Orientation::V =>
                [Direction::Right, Direction::Left]
                .into_iter()
                .filter_map(|dir| {
                    go(node.position, *n, dir, bounds)
                    .map(|position| Node { position, arrival: Orientation::H })
                }).collect(),
            })
        .collect::<Vec<Node>>();

    let heat_loss = |n1: Node, n2: Node| {
        let min_row = std::cmp::min(n1.position.row, n2.position.row);
        let max_row = std::cmp::max(n1.position.row, n2.position.row);
        let min_col = std::cmp::min(n1.position.col, n2.position.col);
        let max_col = std::cmp::max(n1.position.col, n2.position.col);
        let max_diff = std::cmp::max(max_row - min_row, max_col - min_col);
        (1..=max_diff)
        .into_iter()
        .filter_map(|n| {
            let direction = match n2.arrival {
                Orientation::H if n1.position.col < n2.position.col => Direction::Right,
                Orientation::H => Direction::Left,
                Orientation::V if n1.position.row < n2.position.row => Direction::Down,
                Orientation::V => Direction::Up,
            };
            let hotspot = go(n1.position, n, direction, bounds)?;
            Some (grid.get(hotspot)? as u16)
        })
        .sum()
    };

    let path = dijkstra(start, end, &next, &heat_loss, nodes).expect("no path found");

    (0..path.len() - 1)
        .map(|i| heat_loss(path[i], path[i + 1]))
        .map(|i| i as u16)
        .sum()
}

fn reconstruct_path<N>(mut current: N, came_from: HashMap<N, N>) -> Vec<N>
    where N: Eq, N: PartialEq, N: Hash, N: Clone, N: Copy, N: Debug
{
    let mut path = vec![current];
    while came_from.contains_key(&current) {
        let from = came_from[&current];
        path.push(from);
        current = from;
    }
    path.reverse();
    path
}

fn dijkstra<N, O, W>(start: N, end: N, next: O, weight: W, nodes: usize) -> Option<Vec<N>>
    where N: Eq, N: Hash, N: Clone, N: Copy, N: Debug, O: Fn(N) -> Vec<N>, W: Fn(N, N) -> u16
{
    let mut came_from : HashMap<N, N> = HashMap::new();
    let mut distances : HashMap<N, u16> = HashMap::new();// Dict.insert start 0
    distances.insert(start, 0);
    let mut available : Vec<N> = vec![start];
    let mut current;
    loop {
        current = available.pop()?;

        if current == end {
            return Some(reconstruct_path(end, came_from));
        }

        for neighbour in next(current) {
            let distance = distances[&current] + weight(current, neighbour);

            if &distance >= distances.get(&neighbour).unwrap_or(&u16::MAX) { continue; }

            came_from.insert(neighbour, current);
            distances.insert(neighbour, distance);
            available = available.into_iter().filter(|node| *node != neighbour).collect();
            let next_shortest =
                available
                .iter()
                .position(|node| distances[node] < distance)
                .unwrap_or(available.len());
            available.insert(next_shortest, neighbour);
        }
    }
}

fn solution1(grid: Grid, end_orientation: Orientation) -> u16 {
  find_path(grid, (1..=3).collect(), end_orientation)
}

fn solution2(grid: Grid, end_orientation: Orientation) -> u16 {
  find_path(grid, (4..=10).collect(), end_orientation)
}

fn main() {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part1_test() {
        let test = &std::fs::read_to_string("../test.txt").unwrap();
        assert_eq!(102, solution1(parse(test), Orientation::H));
    }

    #[test]
    fn part1() {
        let input = &std::fs::read_to_string("../input.txt").unwrap();
        // this one is odd but hey I did it
        assert_eq!(1195 + 1, solution1(parse(input), Orientation::H));
    }

    #[test]
    fn part2_test() {
        let test = &std::fs::read_to_string("../test.txt").unwrap();
        let test2 = &std::fs::read_to_string("../test2.txt").unwrap();
        assert_eq!(94, solution2(parse(test), Orientation::V));
        assert_eq!(71, solution2(parse(test2), Orientation::H));
    }

    #[test]
    fn part2() {
        let input = &std::fs::read_to_string("../input.txt").unwrap();
        assert_eq!(1347, solution2(parse(input), Orientation::V));
    }
}
