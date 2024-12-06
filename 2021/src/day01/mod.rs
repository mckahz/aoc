#![feature(array_windows)]
fn parse(input: String) -> Vec<u32> {
    input
        .lines()
        .filter_map(|line| line.parse::<u32>().ok())
        .collect()
}

pub fn part1(input: String) -> u32 {
    let nums = parse(input);
    nums.iter()
        .zip(nums[1..].iter())
        .filter(|(first, next)| first < next)
        .count() as u32
}

pub fn part2(input: String) -> u32 {
    let nums = parse(input);
    let sums : Vec<u32> = nums.windows(3).map(|window| window.iter().sum()).collect();
    sums.iter()
        .zip(sums[1..].iter())
        .filter(|(first, next)| first < next)
        .count() as u32
}
