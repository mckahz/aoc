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

Equation : {
    lhs : I64,
    rhs : List I64,
}

Operator : [Add, Mul, Cat, Sub, Div, Trunc]

digits : I64 -> I64
digits = \m -> m |> Num.toStr |> Str.toUtf8 |> List.len |> Num.toI64

endsWith : I64, I64 -> Bool
endsWith = \str, sub ->
    str % Num.powInt 10 (digits sub) == sub

apply : Operator, I64, I64 -> I64
apply = \op, n, m ->
    when op is
        Add -> n + m
        Mul -> n * m
        Cat -> n * Num.powInt 10 (digits m) + m
        Sub -> n - m
        Div -> n // m
        Trunc -> n // (Num.powInt 10 (digits m))

inverse : Operator -> Operator
inverse = \op ->
    when op is
        Add -> Sub
        Mul -> Div
        Cat -> Trunc
        Sub -> Add
        Div -> Mul
        Trunc -> Cat

hasSolution : Equation, List Operator -> Bool
hasSolution = \eq, ops ->
    when eq.rhs is
        [] -> crash "equation cannot have no rhs operands"
        [rhs] -> eq.lhs == rhs
        [.. as rhs, operand] ->
            ops
            |> List.keepIf \op ->
                when op is
                    Add -> eq.lhs - operand > 0
                    Mul -> eq.lhs % operand == 0
                    Cat -> eq.lhs |> endsWith operand
                    _ -> Bool.true
            |> List.any \op ->
                lhs = apply (inverse op) eq.lhs operand
                hasSolution { lhs, rhs } ops

part1 = \input ->
    equations = parse input
    equations
    |> List.keepIf \eq -> eq |> hasSolution [Add, Mul]
    |> List.map .lhs
    |> List.sum

part2 = \input ->
    equations = parse input
    equations
    |> List.keepIf \eq -> eq |> hasSolution [Add, Mul, Cat]
    |> List.map .lhs
    |> List.sum
