module [example, myInput, part1, part2]

import "inputs/day02-example.txt" as example : List U8
import "inputs/day02-input.txt" as myInput : List U8

parse = \input ->
    input
    |> List.splitOn '\n'
    |> List.dropIf List.isEmpty
    |> List.map \line ->
        line
        |> List.splitOn ' '
        |> List.keepOks Str.fromUtf8
        |> List.keepOks Str.toI32

isSafe = \report ->
    pairs = List.map2
        report
        (List.dropFirst report 1)
        \first, next -> (first, next)
    allAsc = List.all pairs \(first, next) -> first < next
    allDesc = List.all pairs \(first, next) -> first > next
    distances =
        List.all pairs \(first, next) ->
            dist = Num.abs (first - next)
            dist >= 1 && dist <= 3
    (allAsc || allDesc) && distances

part1 = \input ->
    parse input
    |> List.countIf isSafe
    |> Num.toI32

isNearlySafe = \report ->
    adjustablySafe =
        List.range { start: At 0, end: At (List.len report) }
        |> List.any \i -> isSafe (List.dropAt report i)
    isSafe report || adjustablySafe

part2 = \input ->
    parse input
    |> List.countIf isNearlySafe
    |> Num.toI32
