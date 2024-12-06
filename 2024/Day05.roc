module [example, myInput, part1, part2]

import "inputs/day05-example.txt" as example : List U8
import "inputs/day05-input.txt" as myInput : List U8

import parser.Parser as P
import parser.String as PS

RuleSet : List (U64, U64)
Node : U64
Seq : List Node

parse : List U8 -> { rules : RuleSet, updates : List Seq }
parse = \input ->
    rules =
        P.const \p1 -> \p2 -> (p1, p2)
        |> P.keep PS.digits
        |> P.skip (PS.codeunit '|')
        |> P.keep PS.digits
        |> P.sepBy (PS.codeunit '\n')

    updates =
        PS.digits
        |> P.sepBy (PS.codeunit ',')
        |> P.sepBy (PS.codeunit '\n')
        |> P.map \us -> List.dropIf us List.isEmpty

    file =
        P.const \r -> \u -> { rules: r, updates: u }
        |> P.keep rules
        |> P.skip (PS.string "\n\n")
        |> P.keep updates

    result =
        when P.parse file input (\_ -> Bool.true) is
            Ok r -> r
            Err e ->
                dbg e
                crash ""

    result

fix : Seq, RuleSet -> Seq
fix = \seq, rules ->
    relevantRules =
        rules
        |> List.keepIf \(lo, hi) ->
            (seq |> List.contains lo)
            && (seq |> List.contains hi)

    countPrevious = \update ->
        relevantRules
        |> List.countIf \(_, hi) -> hi == update

    # indirect sortWith to avoid compiler bug
    seq
    |> List.map \update -> (update, countPrevious update)
    |> List.sortWith \x, y -> Num.compare x.1 y.1
    |> List.map .0

score : Seq -> U64
score = \seq ->
    seq
    |> List.get (List.len seq // 2)
    |> Result.withDefault 0

part1 : List U8 -> U64
part1 = \input ->
    { rules, updates } = parse input

    updates
    |> List.keepIf \seq -> fix seq rules == seq
    |> List.map score
    |> List.sum

part2 : List U8 -> U64
part2 = \input ->
    { rules, updates } = parse input

    updates
    |> List.keepOks \seq ->
        fixed = fix seq rules
        if fixed == seq then Err {} else Ok (score fixed)
    |> List.sum
