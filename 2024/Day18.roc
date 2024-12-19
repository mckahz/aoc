module [example, myInput, part1, part2]

import "inputs/day18-example.txt" as example : List U8
import "inputs/day18-input.txt" as myInput : List U8

import Linear exposing [up, left, down, right]
import Map exposing [Pos]
import Path

Path : Path.Path Pos

parse : List U8 -> List Pos
parse = \input ->
    input
    |> List.dropLast 1
    |> List.splitOn '\n'
    |> List.keepOks \line ->
        nums =
            line
            |> List.splitOn ','
            |> List.keepOks Str.fromUtf8
            |> List.keepOks Str.toI32

        when nums is
            [x, y] -> Ok { x, y }
            _ -> Err InvalidPosition

part1 = \input, end, n ->
    map = parse input |> List.takeFirst n |> Set.fromList

    start = { x: 0, y: 0 }
    getNext = \pos ->
        [left pos, right pos, up pos, down pos]
        |> List.dropIf \p ->
            p.x < 0 || end.x < p.x || p.y < 0 || end.y < p.y
        |> List.dropIf \p ->
            map |> Set.contains p
    cost = \_, _ -> 1
    isEnd = \p -> p == end
    heuristic = \p -> (end.x - p.x) + (end.y - p.y) |> Num.toU64

    Path.shortest start isEnd getNext cost heuristic
    |> List.dropFirst 1
    |> List.len

part2 = \input, end, n ->
    map : Dict Pos U64
    map =
        parse input
        |> List.mapWithIndex \pos, i -> (pos, i)
        |> Dict.fromList

    corruptionAt : Pos, U64 -> Bool
    corruptionAt = \pos, bytes ->
        when map |> Dict.get pos is
            Ok i -> i < bytes
            Err _ -> Bool.false

    cost = \_, _ -> 1
    isEnd = \p -> p == end
    heuristic = \p -> Linear.taxiCab p end |> Num.toU64

    findPath : Pos, U64 -> Result Path [NoPathExists]
    findPath = \start, bytes ->
        getNext = \pos ->
            [left pos, right pos, up pos, down pos]
            |> List.dropIf \p ->
                p.x < 0 || end.x < p.x || p.y < 0 || end.y < p.y
            |> List.dropIf \p ->
                corruptionAt p bytes

        path = Path.shortest start isEnd getNext cost heuristic
        if List.isEmpty path then
            Err NoPathExists
        else
            Ok path

    firstBarricade : Path, U64 -> Pos
    firstBarricade = \path, bytes ->
        firstObstructionResult =
            path
            |> List.findFirstIndex \pos ->
                corruptionAt pos bytes
            |> Result.map \i ->
                before =
                    path
                    |> List.get (i - 1)
                    |> Result.withDefault Linear.zero
                (before, i)

        when firstObstructionResult is
            Err _ -> firstBarricade path (bytes + 1)
            Ok (before, i) ->
                when findPath before bytes is
                    Err _ -> map |> Dict.keys |> List.get (bytes - 1) |> Result.withDefault Linear.zero
                    Ok rest ->
                        firstBarricade (path |> List.takeFirst i |> List.concat rest) (bytes + 1)

    init = findPath Linear.zero n |> Result.withDefault []

    { x, y } = firstBarricade init n

    "$(Num.toStr x),$(Num.toStr y)" |> dbg
