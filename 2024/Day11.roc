module [example1, example2, myInput, part1, part2]

import "inputs/day11-example1.txt" as example1 : List U8
import "inputs/day11-example2.txt" as example2 : List U8
import "inputs/day11-input.txt" as myInput : List U8

Stones : Dict U64 U64

parse = \input ->
    input
    |> List.dropLast 1
    |> List.splitOn ' '
    |> List.keepOks Str.fromUtf8
    |> List.keepOks Str.toU64

blink : U64 -> List U64
blink = \stone ->
    stoneDigits =
        stone
        |> Num.toStr
        |> Str.toUtf8

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

# histories describes when, in terms of the number of
# blinks that had passed, a particular stone was encountered
futures : List U64, Dict U64 (List U64), U64, U64 -> Dict U64 (List U64)
futures = \stones, histories, maxBlinks, blinks ->
    newStones =
        stones
        |> List.joinMap blink

    newHistories =
        newStones
        |> List.walk histories \h, stone ->
            Dict.update h stone \result ->
                when result is
                    Ok occurences -> occurences |> List.append blinks |> Ok
                    Err _ -> Ok [blinks]

    if blinks >= maxBlinks then
        newHistories
    else
        futures (newStones) newHistories maxBlinks (blinks + 1)

part1 = \input, blinks ->
    stones = parse input
    _ =
        futures [2024] (Dict.empty {}) 6 0
        |> Dict.map \stone, history -> dbg (stone, history)
    0

part2 = part1
