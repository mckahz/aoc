interface Day06.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , parser.Core.{ Parser, map, keep, skip, oneOf, sepBy, const, many }
      , parser.String.{ string, parseStr, digits }
      ]

Race : { time: Nat, dist: Nat }

parse : Str -> List Race
parse = \str ->
  parser =
    const \times -> \dists -> 
      List.map2 times dists \time, dist -> { time, dist }
    |> skip (string "Time:") |> skip (many (string " "))
    |> keep (digits |> sepBy (many (string " ")))
    |> skip (string "\nDistance:") |> skip (many (string " "))
    |> keep (digits |> sepBy (many (string " ")))
    |> skip (string "\n")
  when parseStr parser str is
    Ok i -> i
    Err _ -> crash "invalid input"

part1 : Str -> Nat
part1 = \s ->
  races = parse s
  races
  |> List.map \{ time, dist } ->
      List.range { start: After 0, end: Before time }
      |> List.map \t0 -> t0 * (time - t0)
      |> List.countIf \d -> d > dist
  |> List.product

concatNums : List Nat -> Nat
concatNums = \nums ->
  nums
    |> List.map Num.toStr
    |> Str.joinWith ""
    |> Str.toNat
    |> Result.withDefault 0

part2 : Str -> Nat
part2 = \s ->
  races = parse s
  time = races |> List.map .time |> concatNums
  dist = races |> List.map .dist |> concatNums
  b = -(Num.toF64 time)
  c = Num.toF64 dist
  det = b * b - 4 * c
  upper = (-b + Num.sqrt det) / 2
  lower = (-b - Num.sqrt det) / 2
  1 + (Num.floor upper) - (Num.ceiling lower)

# This produces a compiler bug
#part2 : Input -> Nat
#part2 = \race ->
#  a = Num.toDec (-1)
#  b = Num.toDec race.time
#  c = Num.toDec (-race.dist)
#  det = num.sqrt (b * b - 4 * a * c)
#  lower = (-b - det) / (2 * a)
#  upper = (-b + det) / (2 * a)
#  ans = (Num.floor upper) - (Num.ceiling lower)
#  dbg ans
#  0
