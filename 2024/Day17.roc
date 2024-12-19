module [example1, example2, myInput, part1, part2]

import "inputs/day17-example1.txt" as example1 : List U8
import "inputs/day17-example2.txt" as example2 : List U8
import "inputs/day17-input.txt" as myInput : List U8

import parser.Parser as P
import parser.String as PS

Memory : {
    a : U64,
    b : U64,
    c : U64,
}

Op : [Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv]

fromU64 : U64 -> Result Op [InvalidOpCode]
fromU64 = \o ->
    when o is
        0 -> Ok Adv
        1 -> Ok Bxl
        2 -> Ok Bst
        3 -> Ok Jnz
        4 -> Ok Bxc
        5 -> Ok Out
        6 -> Ok Bdv
        7 -> Ok Cdv
        _ -> Err InvalidOpCode

parse = \input ->
    num64 =
        PS.digits |> P.map Num.toU64
    p =
        P.const \a -> \b -> \c -> \program ->
                        ({ a, b, c }, program)
        |> P.skip (PS.string "Register A: ")
        |> P.keep num64
        |> P.skip (PS.string "\nRegister B: ")
        |> P.keep num64
        |> P.skip (PS.string "\nRegister C: ")
        |> P.keep num64
        |> P.skip (PS.string "\n\nProgram: ")
        |> P.keep (num64 |> P.sepBy (PS.codeunit ','))
    result = P.parse p input \_ -> Bool.true
    when result is
        Ok r -> r
        Err e ->
            dbg e
            crash "invalid input"

execute : Memory, List U64, U64 -> List U64
execute = \memory, program, ip ->
    op = program |> List.get ip |> Result.try fromU64
    operand = program |> List.get (ip + 1) |> Result.withDefault 0
    combo = \o ->
        when o is
            0 | 1 | 2 | 3 -> o
            4 -> memory.a
            5 -> memory.b
            6 -> memory.c
            _ -> crash "no"

    when op is
        Err _ -> []
        Ok Adv ->
            d = 2 |> Num.powInt (combo operand)
            execute { memory & a: memory.a // d } program (ip + 2)

        Ok Bxl ->
            execute { memory & b: memory.b |> Num.bitwiseXor operand } program (ip + 2)

        Ok Bst ->
            execute { memory & b: combo operand % 8 } program (ip + 2)

        Ok Jnz if memory.a == 0 -> execute memory program (ip + 2)
        Ok Jnz -> execute memory program operand
        Ok Bxc -> execute { memory & b: memory.b |> Num.bitwiseXor memory.c } program (ip + 2)
        Ok Out ->
            execute memory program (ip + 2)
            |> List.prepend (combo operand % 8)

        Ok Bdv ->
            d = 2 |> Num.powInt (combo operand)
            execute { memory & b: memory.a // d } program (ip + 2)

        Ok Cdv ->
            d = 2 |> Num.powInt (combo operand)
            execute { memory & c: memory.a // d } program (ip + 2)

part1 = \input ->
    (memory, program) = parse input
    execute memory program 0
    |> List.map Num.toStr
    |> Str.joinWith ","

Register : [A, B, C]

Equation : {
    lhs : Expr,
    rhs : Expr,
}

showEq : Equation -> Str
showEq = \eq ->
    "$(showExpr eq.lhs) == $(showExpr eq.rhs)"

Expr : [
    Num U64,
    Register Register,
    ShiftRight Expr Expr,
    Mod8 Expr,
    Xor Expr Expr,
]

showExpr : Expr -> Str
showExpr = \expr ->
    when expr is
        Register A -> "A"
        Register B -> "B"
        Register C -> "C"
        ShiftRight a b -> "($(showExpr a) >> $(showExpr b))"
        Num n -> Num.toStr n
        Xor lhs rhs -> "($(showExpr lhs) ⊕ $(showExpr rhs))"
        Mod8 lhs -> "($(showExpr lhs) % 8)"

register : Register -> Expr
register = \r -> Register r

num : U64 -> Expr
num = \n -> Num n

shiftRight : Expr, Expr -> Expr
shiftRight = \lhs, rhs ->
    when (lhs, rhs) is
        (Num a, Num b) -> Num (a |> Num.shiftRightBy (Num.toU8 b))
        (ShiftRight a (Num b), Num c) -> shiftRight a (Num (b + c))
        _ -> ShiftRight lhs rhs

mod8 : Expr -> Expr
mod8 = \expr ->
    when expr is
        Num n -> num (n % 8)
        _ -> Mod8 expr

xor : Expr, Expr -> Expr
xor = \c1, c2 ->
    when (c1, c2) is
        # (Mod8 a, b) -> mod8 (xor a b)
        (Num 0, _) -> c2
        (_, Num 0) -> c1
        (Num a, Num b) -> num (a |> Num.bitwiseXor b)
        (Xor a b, c) ->
            if b == c then
                a
            else if a == c then
                b
            else
                xor a (xor b c)

        (Num a, Xor (Num b) c) -> xor (num (a |> Num.bitwiseXor b)) c
        (_, _) -> Xor c1 c2

subst : Expr, Register, Expr -> Expr
subst = \expr, r, with ->
    when expr is
        Register r2 if r2 == r -> with
        Register _ -> expr
        Num _ -> expr
        Xor lhs rhs -> xor (subst lhs r with) (subst rhs r with)
        ShiftRight a b -> shiftRight (subst a r with) (subst b r with)
        Mod8 a -> mod8 (subst a r with)

substEq : Equation, Register, Expr -> Equation
substEq = \eq, r, with ->
    { eq & lhs: subst eq.lhs r with, rhs: subst eq.rhs r with }

substAll : List Equation, Register, Expr -> List Equation
substAll = \eqs, r, with ->
    eqs |> List.map \eq -> substEq eq r with

generateEqs : List U64 -> List Equation
generateEqs = \program ->
    generateEqsHelp program [] [] (List.len program - 4)

generateEqsHelp : List U64, List Equation, List U64, U64 -> List Equation
generateEqsHelp = \program, eqs, output, ip ->
    combo = \o ->
        when o is
            0 | 1 | 2 | 3 -> num o
            4 -> register A
            5 -> register B
            6 -> register C
            _ -> crash "no"
    nextOut =
        program
        |> List.takeLast (List.len output + 1)
        |> List.first
        |> Result.withDefault 0

    (inst, op) =
        when program |> List.dropFirst ip |> List.takeFirst 2 is
            [i, o] -> (fromU64 i |> Result.withDefault Jnz, o)
            _ -> crash "invalid"

    newEqs =
        when inst is
            Adv -> eqs |> substAll A (shiftRight (register A) (combo op))
            Bdv -> eqs |> substAll B (shiftRight (register A) (combo op))
            Cdv -> eqs |> substAll C (shiftRight (register A) (combo op))
            Bxl -> eqs |> substAll B (xor (register B) (num op))
            Bxc -> eqs |> substAll B (xor (register B) (register C))
            Bst -> eqs |> substAll B (mod8 (combo op))
            Out ->
                eqs
                |> List.append { lhs: mod8 (combo op), rhs: mod8 (num nextOut) }

            Jnz -> crash "should not encounter"

    printed = inst == Out
    newOutput = if printed then output |> List.prepend nextOut else output

    if List.len newOutput == List.len program && ip == 0 then
        newEqs
    else
        newIp = if ip == 0 then List.len program - 4 else ip - 2
        generateEqsHelp program newEqs newOutput newIp

buildSolution : List Equation -> Result U64 [NoSolution]
buildSolution = \eqs ->
    satisfies = \a, eq ->
        simple = eq |> substEq A (Num a)
        simple.lhs == simple.rhs

    masks3Bit =
        List.range { start: At 0b000, end: At 0b111 }

    eqs
    |> List.walkWithIndex [0] \masks, eq, i ->
        shifts = Num.toU8 (List.len eqs - i - 1)
        masks
        |> List.joinMap \maskLeft ->
            masks3Bit
            |> List.map \mask ->
                mask |> Num.shiftLeftBy (shifts * 3)
            |> List.map \maskRight ->
                maskLeft |> Num.bitwiseOr maskRight
        |> List.keepIf \a ->
            a |> satisfies eq
    |> List.min
    |> Result.mapErr \_ -> NoSolution

part2 = \input ->
    (memory, program) = parse input

    generateEqs program
    |> substAll B (num memory.b)
    |> substAll C (num memory.c)
    |> buildSolution
    |> Result.withDefault 0
