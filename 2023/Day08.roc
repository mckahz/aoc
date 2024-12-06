interface Day08.Solution
    exposes
      [ parse
      , input, test1, test2
      , part1, part2
      , part1Test1Ans, part1Test2Ans, part2Test1Ans
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test1.txt" as test1 : Str
      , "test2.txt" as test2 : Str
      , parser.Core.{ Parser, map, keep, skip, oneOf, sepBy, const, many }
      , parser.String.{ string, parseStr, digits }
      ]

part1Test1Ans = 2
part1Test2Ans = 6
part1Ans = 16531
part2Test1Ans = 6
part2Ans = 24035773251517

Map : { directions: List Direction, nodes: Dict Str (Str, Str), first: Str, last: Str }
Direction: [Left, Right]

parse : Str -> Map
parse = \str ->
  direction =
    oneOf
      [ string "L" |> map \_ -> Left
      , string "R" |> map \_ -> Right
      ]

  directions = many direction

  node =
    alphaNum = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      |> Str.graphemes
      |> List.map string
      |> oneOf

    nodeName =
      many alphaNum |> map \listOfStr -> listOfStr |> Str.joinWith ""

    const \initial -> \left -> \right ->
      (initial, (left, right))
    |> keep nodeName
    |> skip (string " = (")
    |> keep nodeName
    |> skip (string ", ")
    |> keep nodeName
    |> skip (string ")")

  gmap =
    const \dirs -> \nodes ->
      { directions: dirs
      , nodes: nodes |> Dict.fromList
      , first: nodes |> List.first |> Result.map .0 |> Result.withDefault ""
      , last: nodes |> List.last |> Result.map .0 |> Result.withDefault ""
      }
    |> keep directions
    |> skip (string "\n\n")
    |> keep (node |> sepBy (string "\n"))
    |> skip (string "\n")

  result = parseStr gmap str

  when result is
    Ok r -> r
    Err _ ->
      crash ""

countSteps : (Str -> Bool), Map, (Nat, Str), List Direction -> Nat
countSteps = \stopCond, gmap, (count, node), dirs ->
  if stopCond node then
    count
  else
    newDirs =
      if dirs |> List.isEmpty then
        gmap.directions
      else
        dirs
    countSteps stopCond gmap
      ( count + 1
      , when (gmap.nodes |> Dict.get node, newDirs |> List.first) is
          (Ok (left, _), Ok Left) -> left
          (Ok (_, right), Ok Right) -> right
          _ ->
            crash ""
      )
      (newDirs |> List.dropFirst 1)

part1 : Map -> Nat
part1 = \gmap ->
  countSteps (\str -> str == "ZZZ") gmap (0, "AAA") gmap.directions

gcd : Nat, Nat -> Nat
gcd = \n, m ->
  when (n, m) is
    (0, _) -> m
    (_, 0) -> n
    _ ->
      min = Num.min n m
      max = Num.max n m
      gcd min (max % min)

lcm : Nat, Nat -> Nat
lcm = \n, m ->
  when (n, m) is
    (0, 0) -> 0
    _ -> n * m // gcd n m

part2 : Map -> Nat
part2 = \gmap ->
  gmap.nodes
    |> Dict.keys
    |> List.keepIf \nodeName ->
        nodeName |> Str.endsWith "A"
    |> List.map \start ->
        countSteps (\str -> str |> Str.endsWith "Z") gmap (0, start) gmap.directions
    |> List.walk 1 lcm
