module [example1, example2, myInput, part1, part2]

import "inputs/day03-example1.txt" as example1 : List U8
import "inputs/day03-example2.txt" as example2 : List U8
import "inputs/day03-input.txt" as myInput : List U8

import parser.Parser as P
import parser.String as PS

do = PS.string "do()"
dont = PS.string "don't()"
mul =
    P.const (\x -> \y -> (x, y))
    |> P.skip (PS.string "mul(")
    |> P.keep PS.digits
    |> P.skip (PS.string ",")
    |> P.keep PS.digits
    |> P.skip (PS.string ")")

part1 = \input ->
    instruction =
        P.oneOf [
            P.const Mul |> P.keep mul,
            P.const Noop |> P.skip PS.anyCodeunit,
        ]

    discardNoops = \instructions ->
        instructions
        |> List.keepOks \inst ->
            when inst is
                Mul nums -> Ok nums
                _ -> Err {}

    muls =
        P.many instruction
        |> P.map discardNoops

    inputMuls =
        when P.parse muls input (\_ -> Bool.true) is
            Ok r -> r
            Err _ -> crash ""

    inputMuls
    |> List.map \(x, y) -> x * y
    |> List.sum

part2 = \input ->
    instruction =
        P.oneOf [
            P.const Mul |> P.keep mul,
            P.const Dont |> P.skip dont,
            P.const Do |> P.skip do,
            P.const Noop |> P.skip PS.anyCodeunit,
        ]

    instructions =
        when P.parse (P.many instruction) input (\_ -> Bool.true) is
            Ok r -> r
            Err _ -> crash ""

    instructions
    |> List.walk { total: 0, execute: Do } \state, inst ->
        when inst is
            Mul (x, y) if state.execute == Do ->
                { state & total: state.total + x * y }

            Mul _ | Noop -> state
            Do -> { state & execute: Do }
            Dont -> { state & execute: Dont }
    |> .total
