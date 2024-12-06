module [example, myInput, part1, part2]

import "inputs/day01-example.txt" as example : List U8
import "inputs/day01-input.txt" as myInput : List U8

parse = \input ->
    input
    |> List.splitOn '\n'
    |> List.keepOks \line ->
        nums =
            line
            |> List.splitOn ' '
            |> List.keepOks Str.fromUtf8
            |> List.keepOks Str.toI32
        when nums is
            [a, b] -> Ok (a, b)
            _ -> Err ""
    |> List.walk ([], []) \(xs, ys), (x, y) ->
        (List.append xs x, List.append ys y)

part1 = \input ->
    (lhs, rhs) = parse input
    List.map2 (List.sortAsc lhs) (List.sortAsc rhs) Num.sub
    |> List.map Num.abs
    |> List.sum

part2 = \input ->
    (lhs, rhs) = parse input
    lhs
    |> List.map \lnum ->
        count =
            rhs
            |> List.countIf \rnum -> rnum == lnum
            |> Num.toI32
        lnum * count
    |> List.sum
