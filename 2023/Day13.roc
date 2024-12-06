interface Day13.Solution
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
      , Matrix.{ Matrix }
      , Pattern.{ Pattern }
      , Mirror.{ Mirror }
      ]

part1TestAns = 405
part1Ans = 33780
part2TestAns = 400
part2Ans = 23479

parse : Str -> List Pattern
parse = \str ->
  patterns =
    str
      |> Str.split "\n\n"
      |> List.mapTry \pattern ->
          pattern
            |> Str.trim
            |> Pattern.fromStr

  when patterns is
    Ok p -> p
    Err e ->
      dbg e
      crash "invalid pattern"

isFlippedAbout : Pattern, Nat -> Bool
isFlippedAbout = \pattern, n ->
  height = List.len pattern
  minDist = Num.min n (height - n)

  flipped = pattern |> List.reverse

  allRowsMatch = \p1, p2 ->
    List.map2 p1 p2 Pair
      |> List.all \Pair r1 r2 -> r1 == r2

  if n <= height // 2 then
    allRowsMatch (flipped |> List.takeLast (2 * minDist)) pattern
  else
    allRowsMatch flipped (pattern |> List.takeLast (2 * minDist))

findHorizontals : Pattern -> List Mirror
findHorizontals = \pattern ->
  height = List.len pattern
  List.range { start: After 0, end: Before height }
    |> List.keepIf \n ->
        pattern |> isFlippedAbout n
    |> List.map Horizontal

findHorizontal : Pattern -> Result Mirror [NoHorizontalMirror]
findHorizontal = \pattern ->
  findHorizontals pattern
    |> List.first
    |> Result.mapErr \_ -> NoHorizontalMirror

findVerticals : Pattern -> List Mirror
findVerticals = \pattern ->
  findHorizontals (Matrix.transpose pattern)
    |> List.map \mirror ->
        when mirror is
          Horizontal m -> Vertical m
          Vertical _ -> mirror

findVertical : Pattern -> Result Mirror [NoVerticalMirror]
findVertical = \pattern ->
  findVerticals pattern
    |> List.first
    |> Result.mapErr \_ -> NoVerticalMirror

findMirror : Pattern -> Result Mirror [NoMirrors]
findMirror = \pattern ->
  Err E
    |> Result.onErr \_ -> findVertical pattern
    |> Result.onErr \_ -> findHorizontal pattern
    |> Result.mapErr \_ -> NoMirrors

part1 : List Pattern -> Nat
part1 = \patterns ->
  patterns
    |> List.keepOks findMirror
    |> List.map Mirror.score
    |> List.sum

findNewMirror : Pattern -> Result Mirror [NoNewMirror]
findNewMirror = \pattern ->
  vertical = findVertical pattern
  horizontal = findHorizontal pattern

  width = pattern |> List.first |> Result.withDefault [] |> List.len

  patternAsRow = List.join pattern

  List.range { start: At 0, end: Length (List.len patternAsRow) }
    |> List.map \i ->
        patternAsRow
          |> List.update i \land ->
              when land is
                Ash -> Rock
                Rock -> Ash
          |> List.chunksOf width
    |> List.keepOks \possibility ->
        verticals = findVerticals possibility
        horizontals = findHorizontals possibility

        when (verticals, horizontals) is
          ([a], []) if Ok a != vertical -> Ok a
          ([a], [b]) if Ok b == horizontal -> Ok a
          ([a, b], []) if Ok a == vertical -> Ok b
          ([a, b], []) if Ok b == vertical -> Ok a

          ([], [a]) if Ok a != horizontal -> Ok a
          ([a], [b]) if Ok a == vertical -> Ok b
          ([], [a, b]) if Ok a == horizontal -> Ok b
          ([], [a, b]) if Ok b == horizontal -> Ok a

          ([], []) -> Err NotEnoughMirrors
          (_, _) -> Err DidntChangeMirrors
    |> List.first
    |> Result.mapErr \_ -> NoNewMirror

part2 : List Pattern -> Nat
part2 = \patterns ->
  patterns
    |> List.keepOks findNewMirror
    |> List.map Mirror.score
    |> List.sum

