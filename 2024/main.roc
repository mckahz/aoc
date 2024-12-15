app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout

main = Stdout.line! "Hello World!"

# import Day01
# expect Day01.part1 Day01.example == 11
# expect Day01.part1 Day01.myInput == 2580760
# expect Day01.part2 Day01.example == 31
# expect Day01.part2 Day01.myInput == 25358365

# import Day02
# expect Day02.part1 Day02.example == 2
# expect Day02.part1 Day02.myInput == 402
# expect Day02.part2 Day02.example == 4
# expect Day02.part2 Day02.myInput == 455

# import Day03
# expect Day03.part1 Day03.example1 == 161
# expect Day03.part1 Day03.myInput == 189527826
# expect Day03.part2 Day03.example2 == 48
# expect Day03.part2 Day03.myInput == 63013756

# import Day04
# expect Day04.part1 Day04.example == 18
# expect Day04.part1 Day04.myInput == 2569
# expect Day04.part2 Day04.example == 9
# expect Day04.part2 Day04.myInput == 1998

# import Day05
# expect Day05.part1 Day05.example == 143
# expect Day05.part1 Day05.myInput == 5275
# expect Day05.part2 Day05.example == 123
# expect Day05.part2 Day05.myInput == 6191

# TODO: Slow
# import Day06
# expect Day06.part1 Day06.example == 41
# expect Day06.part1 Day06.myInput == 4826
# expect Day06.part2 Day06.example == 6
# expect Day06.part2 Day06.myInput == 1721

# import Day07
# expect Day07.part1 Day07.example == 3749
# expect Day07.part1 Day07.myInput == 8401132154762
# expect Day07.part2 Day07.example == 11387
# expect Day07.part2 Day07.myInput == 95297119227552

# import Day08
# expect Day08.part1 Day08.example == 14
# expect Day08.part1 Day08.myInput == 273
# expect Day08.part2 Day08.example == 34
# expect Day08.part2 Day08.myInput == 1017

# TODO: Slow
# import Day09
# expect Day09.part1 Day09.example == 1928
# expect Day09.part1 Day09.myInput == 6415184586041
# expect Day09.part2 Day09.example == 2858
# expect Day09.part2 Day09.myInput == 6436819084274

# import Day10
# expect Day10.part1 Day10.example == 36
# expect Day10.part1 Day10.myInput == 694
# expect Day10.part2 Day10.example == 81
# expect Day10.part2 Day10.myInput == 1497

# import Day11
# expect Day11.part1 Day11.example1 1 == 7
# expect Day11.part1 Day11.example2 6 == 22
# expect Day11.part1 Day11.example2 25 == 55312
# expect Day11.part1 Day11.myInput 25 == 199753
# expect Day11.part2 Day11.myInput 75 == 239413123020116

# import Day12
# expect Day12.part1 Day12.example1 == 140
# expect Day12.part1 Day12.example2 == 772
# expect Day12.part1 Day12.example3 == 1930
# expect Day12.part1 Day12.myInput == 1483212
# expect Day12.part2 Day12.example4 == 236
# expect Day12.part2 Day12.example5 == 368
# expect Day12.part2 Day12.example3 == 1206
# expect Day12.part2 Day12.myInput == 897062

# import Day13
# expect Day13.part1 Day13.example == 480
# expect Day13.part1 Day13.myInput == 37128
# expect Day13.part2 Day13.myInput == 74914228471331

# import Day14
# expect Day14.part1 Day14.example { x: 11, y: 7 } == 12
# expect Day14.part1 Day14.myInput { x: 101, y: 103 } == 231852216
# expect Day14.part2 Day14.myInput { x: 101, y: 103 } == 8159

import Day15
expect Day15.part1 Day15.example1 == 10092
expect Day15.part1 Day15.example2 == 2028
expect Day15.part1 Day15.myInput == 1430439
expect Day15.part2 Day15.example1 == 9021
expect Day15.part2 Day15.myInput == 1458740
