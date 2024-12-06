interface Day11.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      , part1TestAns, part2TestAns
      , part1Ans, part2Ans
      , solution
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , Universe.{ Universe }
      , Matrix
      ]

part1TestAns = 374
part1Ans = 9233514
part2TestAns = 8410 # 100
part2Ans = 363293506944

parse : Str -> Universe
parse = \str ->
  when Universe.fromStr str is
    Ok universe -> universe
    Err e ->
      dbg e
      crash "invalid input"

solution : Universe, Nat -> Nat
solution = \universe, expansionDistance ->
  emptyRows = universe |> Universe.findEmptyRows
  emptyCols = universe |> Universe.findEmptyCols

  universe
    |> Universe.galaxyPairs
    |> List.map \((r1, c1), (r2, c2)) ->
        expandedRowsBetween =
          emptyRows
            |> List.countIf \row ->
                small = Num.min r1 r2
                big = Num.max r1 r2
                small < row && row < big
        expandedColsBetween =
          emptyCols
            |> List.countIf \col ->
                small = Num.min c1 c2
                big = Num.max c1 c2
                small < col && col < big
        extraSteps =
          [expandedColsBetween, expandedRowsBetween]
            |> List.map \expansions -> expansions * (expansionDistance - 1)
            |> List.sum
        Matrix.taxiCab (r1, c1) (r2, c2) + extraSteps
    |> List.sum

part1 : Universe -> Nat
part1 = \universe ->
  solution universe 2

part2 : Universe -> Nat
part2 = \universe ->
  solution universe 1000000

