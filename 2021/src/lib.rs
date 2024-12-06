fn test(day: u8) -> String {
    let num = if day <= 9 { "0" } else { "" };
    let path = format!("src/day{num}{day}/test.txt");
    std::fs::read_to_string(path.clone()).expect(&format!("{path} not found"))
}

fn input(day: u8) -> String {
    let num = if day <= 9 { "0" } else { "" };
    let path = format!("src/day{num}{day}/input.txt");
    std::fs::read_to_string(path.clone()).expect(&format!("{path} not found"))
}

mod day01;
#[cfg(test)]
mod day01_tests {
    const DAY : u8 = 1;
    use crate::day01::*;
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(7, part1(test(DAY)));
    }
    #[test]
    fn part1_input() {
        assert_eq!(1374, part1(input(DAY)));
    }
    #[test]
    fn part2_test() {
        assert_eq!(5, part2(test(DAY)));
    }
    #[test]
    fn part2_input() {
        assert_eq!(1418, part2(input(DAY)));
    }
}

mod day02;
#[cfg(test)]
mod day02_tests {
    const DAY: u8 = 2;
    use crate::day02::*;
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(150, part1(test(DAY)));
    }
    #[test]
    fn part1_input() {
        assert_eq!(2117664, part1(input(DAY)));
    }
    #[test]
    fn part2_test() {
        assert_eq!(900, part2(test(DAY)));
    }
    #[test]
    fn part2_input() {
        assert_eq!(2073416724, part2(input(DAY)));
    }
}

mod day03;
#[cfg(test)]
mod day03_tests {
    const DAY: u8 = 3;
    use crate::day03::*;
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(198, part1(test(DAY)));
    }
    #[test]
    fn part1_input() {
        assert_eq!(3959450, part1(input(DAY)));
    }
    #[test]
    fn part2_test() {
        assert_eq!(230, part2(test(DAY)));
    }
    #[test]
    fn part2_input() {
        assert_eq!(7440311, part2(input(DAY)));
    }
}

mod day04;
#[cfg(test)]
mod day04_tests {
    const DAY: u8 = 4;
    use crate::day04::*;
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(4512, part1(test(DAY)));
    }
    #[test]
    fn part1_input() {
        assert_eq!(11536, part1(input(DAY)));
    }
    #[test]
    fn part2_test() {
        assert_eq!(1924, part2(test(DAY)));
    }
    #[test]
    fn part2_input() {
        assert_eq!(1284, part2(input(DAY)));
    }
}

mod day05;
#[cfg(test)]
mod day05_tests {
    const DAY: u8 = 5;
    use crate::day05::*;
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(5, part1(test(DAY)));
    }
    #[test]
    fn part1_input() {
        assert_eq!(6267, part1(input(DAY)));
    }
    // #[test]
    // fn part2_test() {
    //     assert_eq!(1924, part2(test(DAY)));
    // }
    // #[test]
    // fn part2_input() {
    //     assert_eq!(1284, part2(input(DAY)));
    // }
}
