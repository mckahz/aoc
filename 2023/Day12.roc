interface Day12.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      , part1TestAns, part2TestAns
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , Row.{ Row }
      ]

part1TestAns = 21
part1Ans = 7361
part2TestAns = 525152
part2Ans = 83317216247365

parse : Str -> List Row
parse = \str ->
  str
    |> Str.split "\n"
    |> List.dropLast 1
    |> List.keepOks Row.fromStr

part1 : List Row -> Nat
part1 = \rows ->
  rows
    |> List.map Row.countArrangements
    |> List.sum

part2 : List Row -> Nat
part2 = \rows ->
  rows
    |> List.map Row.unfold
    |> List.map Row.countArrangements
    |> List.sum
