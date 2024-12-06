use itertools::Itertools;
use nalgebra::*;

type V2 = Vector2<i16>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Line {
    v: V2,
    u: V2,
}

fn str2v2<T>(s: &str) -> Vector2<T>
where
    T: std::str::FromStr,
{
    let mut nums = s.split(",").filter_map(|num| num.parse::<T>().ok());
    let x = nums.next().unwrap();
    let y = nums.next().unwrap();
    Vector2::new(x, y)
}

fn parse(input: String) -> Vec<Line> {
    input
        .lines()
        .filter_map(|line| {
            let mut positions = line.split(" -> ");
            let v = str2v2(positions.next()?);
            let u = str2v2(positions.next()?);
            Some(Line { v, u })
        })
        .collect()
}

impl Line {
    fn len(&self) -> i16 {
        let Line { v, u } = self;
        let diff = (v - u).abs();
        std::cmp::max(diff.x, diff.y) + 1
    }

    fn is_point(&self) -> bool {
        self.v.x == self.u.x && self.v.y == self.u.y
    }

    fn is_horizontal(&self) -> bool {
        self.v.y == self.u.y
    }

    fn is_vertical(&self) -> bool {
        self.v.x == self.u.x
    }

    fn slope(&self) -> Option<i16> {
        if self.u.x - self.v.x == 0 {
            None
        } else {
            Some((self.u.y - self.v.y) / (self.u.x - self.v.x))
        }
    }

    fn intersection(&self, other: &Self) -> Option<Line> {
        if !self.coincides(other) {
            return None;
        } else if self == other {
            return Some(*self)
        }
        let v_between = norm(self.v - other.v) == norm(other.u - self.v);
        let u_between = norm(self.u - other.v) == norm(other.u - self.u);
        const ZERO : V2 = V2::new(0, 0);
        let same_dir = |v: V2, t1: V2, t2: V2| {
            let n1 = norm(t1 - v);
            let n2 = norm(t2 - v);
            n1 == n2
        };
        if self.v == other.v && same_dir() {
        }
        if v_between && u_between {
            Some(*self)
        } else if v_between {
            Some(if same_dir(self.v, self.u, other.v) {
                Line {
                    v: self.v,
                    u: other.v,
                }
            } else {
                Line {
                    v: self.v,
                    u: other.u,
                }
            })
        } else if u_between {
            Some(if same_dir(self.u, self.v, other.v) {
                Line {
                    v: self.u,
                    u: other.v,
                }
            } else {
                Line {
                    v: self.u,
                    u: other.u,
                }
            })
        } else {
            None
        }
    }

    fn coincides(&self, other: &Self) -> bool {
        true
    }

    fn difference(&self, other: &Self) -> Vec<Line> {
        if !self.coincides(other) {
            return vec![*self];
        }
        let between = |v, u| Line {
            v: v + norm(u - v),
            u,
        };
        let v_between = norm(self.v - other.v) == norm(other.u - self.v)
            || self.v == other.v
            || self.v == other.u;
        let u_between = norm(self.u - other.v) == norm(other.u - self.u)
            || self.u == other.v
            || self.u == other.u;
        let closest = |v| {
            if magnitude(v - other.v) < magnitude(v - other.u) {
                other.v
            } else {
                other.u
            }
        };
        if !v_between && !u_between {
            vec![
                between(closest(self.v), self.v),
                between(closest(self.u), self.u),
            ]
        } else if !v_between {
            vec![between(closest(self.v), self.v)]
        } else if !u_between {
            vec![between(closest(self.u), self.u)]
        } else {
            vec![]
        }
    }
}

fn magnitude(v: V2) -> i16 {
    std::cmp::max(v.x.abs(), v.y.abs())
}

fn norm(v: V2) -> V2 {
    V2::new(v.x.signum(), v.y.signum())
}

fn find_intersections(lines: &[Line]) -> Vec<Line> {
    lines
        .iter()
        .enumerate()
        .dropping_back(1)
        .flat_map(|(i, l1)| {
            lines
                .iter()
                .skip(i + 1)
                .filter_map(|l2| l1.intersection(l2))
        })
        .collect()
}

fn remove_intersections(lines: &[Line]) -> Vec<Line> {
    lines
        .iter()
        .enumerate()
        .flat_map(|(i, l1)| {
            lines
                .iter()
                .skip(i + 1)
                .fold(vec![*l1], |non_intersecting, l2| {
                    non_intersecting
                        .into_iter()
                        .flat_map(|l1| l1.difference(l2))
                        .collect()
                })
        })
        .collect()
}

fn count_intersections(lines: &[Line]) -> u32 {
    let intersections: Vec<Line> = find_intersections(&lines);
    let total = (&intersections)
        .into_iter()
        .map(|intersection| intersection.len() as u32)
        .sum::<u32>();
    total
}

pub fn part1(input: String) -> u32 {
    let lines = parse(input);
    let flat_lines: Vec<Line> = lines
        .into_iter()
        .filter(|line| line.is_vertical() || line.is_horizontal())
        .collect();
    count_intersections(&flat_lines)
}

pub fn part2(input: String) -> u32 {
    let lines = parse(input);
    count_intersections(&lines)
}
