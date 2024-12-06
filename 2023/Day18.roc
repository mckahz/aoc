interface Day18.Solution
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
      , DigPlan.{ DigPlan }
      , Boundary.{ Boundary, Coord }
      ]

part1TestAns = 62
part1Ans = 41019
part2TestAns = 952408144115
part2Ans = 96116995735219

parse : Str -> DigPlan
parse = \str ->
  when DigPlan.fromStr str is
    Ok dp -> dp
    Err e ->
      dbg e
      crash "invalid input"

hash = "#"

part1 : DigPlan -> Nat
part1 = \digPlan ->
  digPlan
  |> Boundary.fromDigPlan
  |> Boundary.countDugOut

part2 : DigPlan -> Nat
part2 = \digPlan ->
  digPlan
  |> DigPlan.reinterpret
  |> Boundary.fromDigPlan
  |> Boundary.countDugOut
