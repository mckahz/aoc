# this causes a stack overflow for some reason
interface Day17.Solution
    exposes
      [ parse
      , input, test1, test2
      , part1, part2
      , part1Test1Ans, part2Test1Ans, part2Test2Ans
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test1.txt" as test1 : Str
      , "test2.txt" as test2 : Str
      , Path
      ]

part1Test1Ans = 102 # H
part1Ans = 1195 + 1 # H
part2Test1Ans = 94 # V
part2Test2Ans = 71 # H
part2Ans = 1347 # V

Node :
  { position: Position
  , orientation: [H, V]
  }
Grid :
  { bounds : Bounds
  , values : List (List U8)
  }
Direction : [Left, Right, Up, Down]
Bounds :
  { bottom : U8
  , right : U8
  }
Position :
  { row: U8
  , col : U8
  }

parse : Str -> Grid
parse = \str ->
  values = str
    |> Str.split "\n"
    |> List.dropLast 1
    |> List.map \line ->
        line
        |> Str.graphemes
        |> List.keepOks Str.toU8

  bottom =
    values
    |> List.len
    |> Num.sub 1
    |> Num.toU8

  right =
    values
    |> List.first
    |> Result.map List.len
    |> Result.withDefault 0
    |> Num.sub 1
    |> Num.toU8

  bounds = { bottom, right }

  { values, bounds }

move : Position, U8, Direction, Bounds -> Result Position [OutOfBounds]
move = \p, n, direction, b ->
  when direction is
    Left if p.col < n -> Err OutOfBounds
    Up if p.row < n -> Err OutOfBounds
    Right if p.col + n > b.right -> Err OutOfBounds
    Down if p.row + n > b.bottom -> Err OutOfBounds
    Left -> Ok { p & col: p.col - n }
    Up -> Ok { p & row: p.row - n }
    Right -> Ok { p & col: p.col + n }
    Down -> Ok { p & row: p.row + n }

get : Grid, Position -> Result U8 [OutOfBounds]
get = \grid, pos ->
  row <- Result.try (grid.values |> List.get (Num.toNat pos.row))
  row |> List.get (Num.toNat pos.col)

findPath : Grid, List U8, [H, V] -> Nat
findPath = \grid, distances, endOrientation ->
  bounds = grid.bounds

  start = { position: { row: 0, col: 0 }, orientation: V }
  end = { position: { row: bounds.bottom, col: bounds.right }, orientation: endOrientation }

  next : Node -> List Node
  next = \node ->
    n <- List.joinMap distances

    (directions, orientation) =
      when node.orientation is
        H -> ([Up, Down], V)
        V -> ([Left, Right], H)

    dir <- List.keepOks directions
    node.position
    |> move n dir bounds
    |> Result.map \position -> { position , orientation }

  heatLoss : Node, Node -> Nat
  heatLoss = \n1, n2 ->
    minRow = Num.min n1.position.row n2.position.row
    maxRow = Num.max n1.position.row n2.position.row
    minCol = Num.min n1.position.col n2.position.col
    maxCol = Num.max n1.position.col n2.position.col
    maxDiff = Num.max (maxRow - minRow) (maxCol - minCol)

    List.range { start: At 1, end: At maxDiff }
    |> List.keepOks \n ->
        direction = when n2.orientation is
          H -> if n1.position.col < n2.position.col then Right else Left
          V -> if n1.position.row < n2.position.row then Down else Up
        hotspot <- Result.try (move n1.position n direction bounds)
        value <- Result.try (grid |> get hotspot)
        Ok (Num.toNat value)
    |> List.sum

  path = when Path.dijkstra start end next heatLoss is
    Ok p -> p
    Err e -> crash "no path found"

  List.map2 path (path |> List.dropFirst 1) heatLoss
  |> List.sum

part1 : Grid, [H, V] -> Nat
part1 = \grid, orientation ->
  findPath grid (List.range { start: At 1, end: At 3 }) orientation

part2 : Grid, [H, V] -> Nat
part2 = \grid, orientation ->
  findPath grid (List.range { start: At 4, end: At 10 }) orientation
