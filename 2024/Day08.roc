module [example, myInput, part1, part2]

import "inputs/day08-example.txt" as example : List U8
import "inputs/day08-input.txt" as myInput : List U8

Pos : (I32, I32)

Map : {
    nodes : List (List Pos),
    width : I32,
    height : I32,
}

gcd = \a, b ->
    if b == 0 then
        a
    else
        gcd b (a % b)

deduplicate = \list ->
    list
    |> Set.fromList
    |> Set.toList

maxMapSize = 50

parse : List U8 -> Map
parse = \input ->
    width =
        input
        |> List.walkUntil 0 \w, c ->
            when c is
                '\n' -> Break w
                _ -> Continue (w + 1)
        |> Num.toI32

    height =
        input
        |> List.countIf \c -> c == '\n'
        |> Num.toI32

    nodePositions =
        input
        |> List.splitOn '\n'
        |> List.dropLast 1
        |> List.mapWithIndex \line, y ->
            line
            |> List.mapWithIndex \char, x ->
                (char, (Num.toI32 x, Num.toI32 y))
        |> List.join

    nodes =
        nodePositions
        |> List.map .0
        |> deduplicate
        |> List.dropIf \n -> n == '.'
        |> List.map \node ->
            nodePositions
            |> List.keepIf \(n, _) -> n == node
            |> List.map .1

    { nodes, width, height }

findAllAntinodes : (Pos, Pos -> List Pos), List Pos -> List Pos
findAllAntinodes = \find, nodes ->
    n1 <- nodes |> List.joinMap
    n2 <- nodes |> List.joinMap
    if n1 == n2 then
        []
    else
        find n1 n2

findAntinodes1 : Pos, Pos -> List Pos
findAntinodes1 = \(x1, y1), (x2, y2) ->
    dx = x2 - x1
    dy = y2 - y1
    [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]

part1 = \input ->
    map = parse input

    map.nodes
    |> List.map \positions -> findAllAntinodes findAntinodes1 positions
    |> List.join
    |> List.keepIf \(x, y) ->
        0 <= x && x < map.width && 0 <= y && y < map.height
    |> deduplicate
    |> List.len

findAntinodes2 : Pos, Pos -> List Pos
findAntinodes2 = \(x1, y1), (x2, y2) ->
    odx = x2 - x1
    ody = y2 - y1
    dx = odx // gcd odx ody
    dy = ody // gcd odx ody

    count = Num.max (maxMapSize // Num.toU64 dx) (maxMapSize // Num.toU64 dy)
    List.range { start: At 1, end: Length count }
    |> List.joinMap \n -> [(x1 - n * dx, y1 - n * dy), (x2 + n * dx, y2 + n * dy)]

part2 = \input ->
    map = parse input

    map.nodes
    |> List.map \positions -> findAllAntinodes findAntinodes2 positions
    |> List.join
    |> List.keepIf \(x, y) ->
        0 <= x && x < map.width && 0 <= y && y < map.height
    |> deduplicate
    |> List.len
