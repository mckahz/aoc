module [example, myInput, part1, part2]

import "inputs/day10-example.txt" as example : List U8
import "inputs/day10-input.txt" as myInput : List U8

parse = \input ->
    input
    |> List.splitOn '\n'
    |> List.dropLast 1
    |> List.map \line ->
        line
        |> List.map List.single
        |> List.keepOks Str.fromUtf8
        |> List.keepOks Str.toU8

Map : List (List U8)

trailheads : Map -> List Pos
trailheads = \map ->
    map
    |> List.mapWithIndex \row, y ->
        row
        |> List.mapWithIndex \n, x ->
            if n == 0 then
                [(Num.toI32 x, Num.toI32 y)]
            else
                []
        |> List.join
    |> List.join

get : Map, Pos -> Result U8 [OutOfBounds]
get = \map, (x, y) ->
    map
    |> List.get? (Num.toU64 y)
    |> List.get (Num.toU64 x)

Pos : (I32, I32)

left : Pos -> Pos
left = \(x, y) -> (x - 1, y)

right : Pos -> Pos
right = \(x, y) -> (x + 1, y)

up : Pos -> Pos
up = \(x, y) -> (x, y - 1)

down : Pos -> Pos
down = \(x, y) -> (x, y + 1)

findEndAllPaths : Pos, Map -> Set Pos
findEndAllPaths = \pos, map ->
    curN = map |> get pos |> Result.withDefault 0
    if curN == 9 then
        Set.single pos
    else
        [left pos, right pos, up pos, down pos]
        |> Set.fromList
        |> Set.keepIf \adjPos ->
            adjN = map |> get adjPos |> Result.withDefault 0
            adjN == curN + 1
        |> Set.joinMap \adjPos ->
            findEndAllPaths adjPos map

part1 = \input ->
    map = parse input
    trailheads map
    |> List.map \trailhead ->
        findEndAllPaths trailhead map
    |> List.map Set.len
    |> List.sum

countPaths : Pos, Map -> U64
countPaths = \pos, map ->
    curN = map |> get pos |> Result.withDefault 0
    if curN == 9 then
        1
    else
        [left pos, right pos, up pos, down pos]
        |> List.keepIf \adjPos ->
            adjN = map |> get adjPos |> Result.withDefault 0
            adjN == curN + 1
        |> List.map \adjPos ->
            countPaths adjPos map
        |> List.sum

part2 = \input ->
    map = parse input
    trailheads map
    |> List.map \trailhead ->
        countPaths trailhead map
    |> List.sum
