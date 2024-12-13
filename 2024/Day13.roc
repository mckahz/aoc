module [example, myInput, part1, part2]

import "inputs/day13-example.txt" as example : List U8
import "inputs/day13-input.txt" as myInput : List U8

import parser.Parser as P
import parser.String as PS

ClawMachine : {
    aButton : (U64, U64),
    bButton : (U64, U64),
    prize : (U64, U64),
}

Matrix : {
    m11 : F64,
    m21 : F64,
    m12 : F64,
    m22 : F64,
}

tol = { atol: 0.001, rtol: 0.001 }

det : Matrix -> F64
det = \m ->
    m.m11 * m.m22 - m.m12 * m.m21

inv : Matrix -> Result Matrix [MatrixNotInversable]
inv = \m ->
    d = det m
    if d |> Num.isApproxEq 0 tol then
        Err MatrixNotInversable
    else
        Ok {
            m11: m.m22 / d,
            m21: (-m.m21) / d,
            m12: (-m.m12) / d,
            m22: m.m11 / d,
        }

parse : List U8 -> List ClawMachine
parse = \input ->
    button = \s ->
        P.const \x -> \y -> (x, y)
        |> P.skip (PS.string "Button $(s): X+")
        |> P.keep PS.digits
        |> P.skip (PS.string ", Y+")
        |> P.keep PS.digits

    prize =
        P.const \x -> \y -> (x, y)
        |> P.skip (PS.string "Prize: X=")
        |> P.keep PS.digits
        |> P.skip (PS.string ", Y=")
        |> P.keep PS.digits

    clawMachines =
        P.const \a -> \b -> \p ->
                    { aButton: a, bButton: b, prize: p }
        |> P.keep (button "A")
        |> P.skip (PS.codeunit '\n')
        |> P.keep (button "B")
        |> P.skip (PS.codeunit '\n')
        |> P.keep prize
        |> P.sepBy (PS.string "\n\n")

    result = P.parse clawMachines input (\_ -> Bool.true)
    when result is
        Ok r -> r
        Err e ->
            dbg e
            crash "invalid input"

toCoefMatrix : ClawMachine -> Matrix
toCoefMatrix = \m -> {
    m11: m.aButton.0 |> Num.toF64,
    m21: m.aButton.1 |> Num.toF64,
    m12: m.bButton.0 |> Num.toF64,
    m22: m.bButton.1 |> Num.toF64,
}

pressesNeeded : ClawMachine -> Result { a : U64, b : U64 } [NoWayToWin]
pressesNeeded = \cm ->
    m = cm |> toCoefMatrix
    inv m
    |> Result.mapErr \_ -> NoWayToWin
    |> Result.try \i ->
        { prize } = cm
        fa = i.m11 * (Num.toF64 prize.0) + i.m12 * (Num.toF64 prize.1)
        fb = i.m21 * (Num.toF64 prize.0) + i.m22 * (Num.toF64 prize.1)

        if (fa < 0) || (fb < 0) then
            Err NoWayToWin
        else
            Ok (fa, fb)
    |> Result.try \(fa, fb) ->
        ia = Num.round fa
        ib = Num.round fb
        fx = \{} -> ia * cm.aButton.0 + ib * cm.bButton.0
        fy = \{} -> ia * cm.aButton.1 + ib * cm.bButton.1
        if (cm.prize.0 != fx {}) || (cm.prize.1 != fy {}) then
            Err NoWayToWin
        else
            Ok { a: ia, b: ib }

minToWin : List ClawMachine -> U64
minToWin = \clawMachines ->
    clawMachines
    |> List.keepOks \m ->
        { a, b } = pressesNeeded? m
        Ok (3 * a + b)
    |> List.sum

part1 = \input ->
    clawMachines = parse input
    minToWin clawMachines

part2 = \input ->
    clawMachines =
        parse input
        |> List.map \clawMachine ->
            boost = 10000000000000
            { clawMachine &
                prize: (
                    clawMachine.prize.0 + boost,
                    clawMachine.prize.1 + boost,
                ),
            }
    minToWin clawMachines
