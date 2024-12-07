module [example, myInput, part1, part2]

import "inputs/day07-example.txt" as example : List U8
import "inputs/day07-input.txt" as myInput : List U8

parse = \input ->
    input
    |> List.splitOn '\n'
    |> List.dropLast 1
    |> List.map \line ->
        when line |> List.splitOn ':' is
            [result, operands] ->
                {
                    lhs: result
                    |> Str.fromUtf8
                    |> Result.try Str.toI64
                    |> Result.withDefault 0,
                    rhs: operands
                    |> List.splitOn ' '
                    |> List.dropIf List.isEmpty
                    |> List.keepOks Str.fromUtf8
                    |> List.map Str.trim
                    |> List.keepOks Str.toI64,
                }

            _ ->
                crash "invalid input"

Operator : I64, I64 -> I64

Equation : {
    lhs : I64,
    rhs : List I64,
}

hasSolution : Equation, List Operator -> Bool
hasSolution = \eq, ops ->
    operatorCount = List.len eq.rhs - 1
    base = List.len ops
    operatorMasks =
        List.range { start: At 0, end: Length (base |> Num.powInt operatorCount) }

    operatorPositions =
        List.range { start: At (operatorCount - 1), end: At 0 }

    List.any operatorMasks \mask ->
        operators =
            List.keepOks operatorPositions \i ->
                index = mask // (base |> Num.powInt i) % base
                ops |> List.get index

        (firstOperand, restOperands) =
            when eq.rhs is
                [x, .. as xs] -> (x, xs)
                _ -> crash "single operand equation"

        result =
            List.map2 operators restOperands \x, y -> (x, y)
            |> List.walk firstOperand \acc, (op, operand) ->
                op acc operand

        eq.lhs == result

part1 = \input ->
    input
    |> parse
    |> List.keepIf \eq -> eq |> hasSolution [Num.add, Num.mul]
    |> List.map .lhs
    |> List.sum

concat : I64, I64 -> I64
concat = \a, b ->
    when Str.concat (Num.toStr a) (Num.toStr b) |> Str.toI64 is
        Ok c -> c
        Err _ -> crash ""

part2 = \input ->
    input
    |> parse
    |> List.keepIf \eq -> eq |> hasSolution [Num.add, Num.mul, concat]
    |> List.map .lhs
    |> List.sum
