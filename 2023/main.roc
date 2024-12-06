app "Advent Of Code 2023"
  packages {
    pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
  }
  imports
    [ pf.Task.{ Task }
    , pf.Stdout

#   , Cache
#   , Graph
#   , Math
#   , Matrix
#   , Path
#   , Parsing
#   , Random
#   , Util

#   , Day01.Solution
#   , Day02.Solution
#   , Day03.Solution
#   , Day04.Solution
#   , Day05.Solution
#   , Day06.Solution
   , Day07.Solution
#   , Day08.Solution
#   , Day09.Solution
#   , Day10.Solution
#   , Day11.Solution
#   , Day12.Solution
#   , Day13.Solution
#   , Day14.Solution
#   , Day15.Solution
#   , Day16.Solution
#   , Day17.Solution
#   , Day18.Solution
#   , Day19.Solution
#   , Day20.Solution
#   , Day21.Solution
#   , Day22.Solution
#   , Day23.Solution
#   , Day24.Solution
#   , Day25.Solution
    ]
    provides [main] to pf


# expect Day01.Solution.part1 Day01.Solution.test1 == 142
# expect Day01.Solution.part1 Day01.Solution.input == 55130
# expect Day01.Solution.part2 Day01.Solution.test2 == 281
# expect Day01.Solution.part2 Day01.Solution.input == 54985


# expect Day02.Solution.part1 Day02.Solution.test  == 8
# expect Day02.Solution.part1 Day02.Solution.input == 2076
# expect Day02.Solution.part2 Day02.Solution.test  == 2286
# expect Day02.Solution.part2 Day02.Solution.input == 70950


# expect Day03.Solution.part1 Day03.Solution.test  == 4361
# expect Day03.Solution.part1 Day03.Solution.input == 533775
# expect Day03.Solution.part2 Day03.Solution.test  == 467835
# expect Day03.Solution.part2 Day03.Solution.input == 78236071


# expect Day04.Solution.part1 Day04.Solution.test  == 13
# expect Day04.Solution.part1 Day04.Solution.input == 21919
# expect Day04.Solution.part2 Day04.Solution.test  == 30
# expect Day04.Solution.part2 Day04.Solution.input == 9881048


# expect Day05.Solution.part1 Day05.Solution.test  == 35
# expect Day05.Solution.part1 Day05.Solution.input == 806029445
# expect Day05.Solution.part2 Day05.Solution.test  == 46
# expect Day05.Solution.part2 Day05.Solution.input == 59370572


# expect Day06.Solution.part1 Day06.Solution.test  == 288
# expect Day06.Solution.part1 Day06.Solution.input == 2065338
# expect Day06.Solution.part2 Day06.Solution.test  == 71503
# expect Day06.Solution.part2 Day06.Solution.input == 34934171


expect Day07.Solution.part1 Day07.Solution.test  == 6440
expect Day07.Solution.part1 Day07.Solution.input == 248569531
expect Day07.Solution.part2 Day07.Solution.test  == 5905
expect Day07.Solution.part2 Day07.Solution.input == 250382098


# expect Day08.Solution.part1 Day08.Solution.test  == 8
# expect Day08.Solution.part1 Day08.Solution.input == 2076
# expect Day08.Solution.part2 Day08.Solution.test  == 2286
# expect Day08.Solution.part2 Day08.Solution.input == 70950


# expect Day09.Solution.part1 Day09.Solution.test  == 8
# expect Day09.Solution.part1 Day09.Solution.input == 2076
# expect Day09.Solution.part2 Day09.Solution.test  == 2286
# expect Day09.Solution.part2 Day09.Solution.input == 70950
#
# expect  == Day10.Solution.part1 Day10.Solution.test1
# expect  == Day10.Solution.part1 Day10.Solution.test2
# expect  == Day10.Solution.part1 Day10.Solution.input
# expect  == Day10.Solution.part2 Day10.Solution.test1
# expect  == Day10.Solution.part2 Day10.Solution.test2
# expect  == Day10.Solution.part2 Day10.Solution.test3
# expect  == Day10.Solution.part2 Day10.Solution.test4
# expect  == Day10.Solution.part2 Day10.Solution.test5
# expect  == Day10.Solution.part2 Day10.Solution.test6
# expect  == Day10.Solution.part2 Day10.Solution.input
#
# expect Day11.Solution.part1 Day11.Solution.test  == 8
# expect Day11.Solution.part1 Day11.Solution.input == 2076
# expect Day11.Solution.part2 Day11.Solution.test  == 2286
# expect Day11.Solution.part2 Day11.Solution.input == 70950


# expect Day12.Solution.part1 Day12.Solution.test  == 8
# expect Day12.Solution.part1 Day12.Solution.input == 2076
# expect Day12.Solution.part2 Day12.Solution.test  == 2286
# expect Day12.Solution.part2 Day12.Solution.input == 70950


# expect Day13.Solution.part1 Day13.Solution.test  == 8
# expect Day13.Solution.part1 Day13.Solution.input == 2076
# expect Day13.Solution.part2 Day13.Solution.test  == 2286
# expect Day13.Solution.part2 Day13.Solution.input == 70950


# expect Day14.Solution.part1 Day14.Solution.test  == 8
# expect Day14.Solution.part1 Day14.Solution.input == 2076
# expect Day14.Solution.part2 Day14.Solution.test  == 2286
# expect Day14.Solution.part2 Day14.Solution.input == 70950


# expect Day15.Solution.part1 Day15.Solution.test  == 8
# expect Day15.Solution.part1 Day15.Solution.input == 2076
# expect Day15.Solution.part2 Day15.Solution.test  == 2286
# expect Day15.Solution.part2 Day15.Solution.input == 70950


# expect Day16.Solution.part1 Day16.Solution.test  == 8
# expect Day16.Solution.part1 Day16.Solution.input == 2076
# expect Day16.Solution.part2 Day16.Solution.test  == 2286
# expect Day16.Solution.part2 Day16.Solution.input == 70950


# expect Day17.Solution.part1 Day17.Solution.test1 H == 
# expect Day17.Solution.part1 Day17.Solution.input H == 
# expect Day17.Solution.part2 Day17.Solution.test1 V == 
# expect Day17.Solution.part2 Day17.Solution.test2 H == 
# expect Day17.Solution.part2 Day17.Solution.input V == 


# expect Day18.Solution.part1 Day18.Solution.test  == 8
# expect Day18.Solution.part1 Day18.Solution.input == 2076
# expect Day18.Solution.part2 Day18.Solution.test  == 2286
# expect Day18.Solution.part2 Day18.Solution.input == 70950


# skipping because the numbers are too large for Nat interface
# expect Day19.Solution.part1 Day19.Solution.test  == 
# expect Day19.Solution.part1 Day19.Solution.input == 
# expect Day19.Solution.part2 Day19.Solution.test  == 
# expect Day19.Solution.part2 Day19.Solution.input == 


# TODO: remove this
# skipping because the compiler hangs when I include this import
# expect Day20.Solution.part1 Day20.Solution.test  == 
# expect Day20.Solution.part1 Day20.Solution.input == 
# expect Day20.Solution.part2 Day20.Solution.test  == 
# expect Day20.Solution.part2 Day20.Solution.input == 


# expect  Day21.Solution.part1 Day21.Solution.test  == 6
# expect  Day21.Solution.part1 Day21.Solution.input == 64
# expect  Day21.Solution.part2 Day21.Solution.input == 26501365


# expect Day22.Solution.part1 Day22.Solution.test  == 8
# expect Day22.Solution.part1 Day22.Solution.input == 2076
# expect Day22.Solution.part2 Day22.Solution.test  == 2286
# expect Day22.Solution.part2 Day22.Solution.input == 70950


# expect Day23.Solution.part1 Day23.Solution.test  == 8
# expect Day23.Solution.part1 Day23.Solution.input == 2076
# expect Day23.Solution.part2 Day23.Solution.test  == 2286
# expect Day23.Solution.part2 Day23.Solution.input == 70950


# expect Day24.Solution.part1 (7, 27) Day24.Solution.test
# expect Day24.Solution.part1 (200000000000000, 400000000000000) Day24.Solution.input
# expect Day24.Solution.part2 Day24.Solution.test
# expect Day24.Solution.part2 Day24.Solution.input


# expect Day25.Solution.part1 Day25.Solution.test  == 8
# expect Day25.Solution.part1 Day25.Solution.input == 2076

# TODO: find out which day this was for
# skipping part 2 evaluation because gauss jordan isn't implemented

main : Task {} *
main = Stdout.line "Advent Of Code 2023"
