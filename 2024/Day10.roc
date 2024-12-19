module [example, myInput, part1, part2]

import "inputs/day10-example.txt" as example : List U8
import "inputs/day10-input.txt" as myInput : List U8

import Linear exposing [left, right, up, down]
import Map exposing [Pos]

Map : Map.Map U8

parse = \input ->
    input
    |> Map.parse \c ->
        [c]
        |> Str.fromUtf8
        |> Result.try Str.toU8
        |> Result.map Cell
        |> Result.withDefault Empty

trailheads : Map -> List Pos
trailheads = \map ->
    map |> Map.findAll 0 |> Set.toList

findEndAllPaths : Pos, Map -> Set Pos
findEndAllPaths = \pos, map ->
    curN = map |> Map.get pos |> Result.withDefault 0
    if curN == 9 then
        Set.single pos
    else
        [left pos, right pos, up pos, down pos]
        |> Set.fromList
        |> Set.keepIf \adjPos ->
            adjN = map |> Map.get adjPos |> Result.withDefault 0
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
    curN = map |> Map.get pos |> Result.withDefault 0
    if curN == 9 then
        1
    else
        [left pos, right pos, up pos, down pos]
        |> List.keepIf \adjPos ->
            adjN = map |> Map.get adjPos |> Result.withDefault 0
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
