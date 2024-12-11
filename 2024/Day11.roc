module [example1, example2, myInput, part1, part2]

import "inputs/day11-example1.txt" as example1 : List U8
import "inputs/day11-example2.txt" as example2 : List U8
import "inputs/day11-input.txt" as myInput : List U8

StoneCounts : Dict U64 U64

parse : List U8 -> StoneCounts
parse = \input ->
    input
    |> List.dropLast 1
    |> List.splitOn ' '
    |> List.keepOks Str.fromUtf8
    |> List.keepOks Str.toU64
    |> List.map \stone -> (stone, 1)
    |> Dict.fromList

blinkOne : U64 -> List U64
blinkOne = \stone ->
    stoneDigits = stone |> Num.toStr |> Str.toUtf8

    if stone == 0 then
        [1]
    else if Num.isEven (List.len stoneDigits) then
        { before: lhs, others: rhs } =
            stoneDigits
            |> List.splitAt (List.len stoneDigits // 2)
        [lhs, rhs]
        |> List.keepOks Str.fromUtf8
        |> List.keepOks Str.toU64
    else
        [stone * 2024]

blink : StoneCounts -> StoneCounts
blink = \stoneCounts ->
    stoneCounts
    |> Dict.toList
    |> List.joinMap \(current, n) ->
        blinkOne current
        |> List.map \next -> (next, n)
    |> List.walk (Dict.empty {}) \acc, (next, n) ->
        Dict.update acc next \result ->
            when result is
                Ok m -> Ok (m + n)
                Err Missing -> Ok n

part1 = \input, blinks ->
    stoneCounts = parse input

    List.repeat blink blinks
    |> List.walk stoneCounts \s, b -> b s
    |> Dict.values
    |> List.sum

part2 = part1
