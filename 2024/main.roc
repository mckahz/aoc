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

import Day06
expect Day06.part1 Day06.example == 41
expect Day06.part1 Day06.myInput == 4826
expect Day06.part2 Day06.example == 6
expect Day06.part2 Day06.myInput == 1721

import Day07
expect Day07.part1 Day07.example == 3749
expect Day07.part1 Day07.myInput == 8401132154762
expect Day07.part2 Day07.example == 11387
expect Day07.part2 Day07.myInput == 95297119227552
