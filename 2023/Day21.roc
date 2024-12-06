interface Day21.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      , part1TestAns
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , Map.{ Map, Position }
      , Math
      ]

part1TestAns = 16 # 6
part1Ans = 3542 # 64
# the input for part 2 doesn't follow the same rules as the actual input
part2Ans = 593174122420825 # 26501365

parse : Str -> Map
parse = \str ->
  when str |> Map.fromStr is
    Ok map -> map
    Err e ->
      dbg e
      crash "invalid input"

step : Map, Nat -> List (Nat, Nat)
step = \map, steps ->
  iter : Set Position, Nat, List (Nat, Nat) -> List (Nat, Nat)
  iter = \positions, stepsTaken, possibilities ->
    dbg (steps - stepsTaken)
    if stepsTaken >= steps then
      possibilities
    else
      newPositions =
        position <- Set.joinMap positions
        direction <- Set.joinMap (Set.fromList [Up, Left, Down, Right])

        when Map.safelyStep position map direction is
          Ok newPosition -> Set.single newPosition
          Err Obstructed | Err OutOfBounds -> Set.empty {}

      iter
        newPositions
        (stepsTaken + 1)
        ( possibilities
          |> List.append (stepsTaken + 1, Set.len newPositions)
        )

  iter (Set.single map.start) 0 []

part1 : Nat, Map -> Nat
part1 = \steps, map ->
  when step map steps is
    [.., (s, n)] if s == steps -> n
    _ -> crash "wrong number of inputs"

part2 : Nat, Map -> Nat
part2 = \steps, map ->
  i = map.size // 2
  j = map.size + (map.size // 2)
  k = 2 * map.size + (map.size // 2)

  threePoints =
    step map k
    |> List.keepIf \(s, _) -> [i, j, k] |> List.contains s
    |> List.map .1

  when threePoints is
    [] | [_] | [_, _] | [_, _, _, _, ..] ->
      crash "there will always be 3 inputs"

    [x, y, z] ->
      a = (z - 2 * y + x) // 2
      b = y - x - a
      c = x
      n = (steps - (map.size // 2)) // map.size

      (a * n * n) + b * n + c
