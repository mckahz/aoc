module [example1, example2, myInput, part1, part2]

import "inputs/day17-example1.txt" as example1 : List U8
import "inputs/day17-example2.txt" as example2 : List U8
import "inputs/day17-input.txt" as myInput : List U8

import parser.Parser as P
import parser.String as PS

numLog2 : U64 -> U64
numLog2 = \n ->
    if n == 0 then
        0
    else
        1 + numLog2 (n |> Num.shiftRightBy 1)

Register : [A, B, C]

Memory : {
    a : U64,
    b : U64,
    c : U64,
}

# op

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

# eq

Equation : {
    lhs : Expr,
    rhs : Expr,
    mod : [Mod8, Standard],
    type : [Lt, Lte, Equals, Gte, Gt],
}

showEq : Equation -> Str
showEq = \eq ->
    symbol =
        when eq.type is
            Lt -> "<"
            Lte -> "<="
            Equals -> "=="
            Gte -> ">="
            Gt -> ">"
    e = "$(showExpr eq.lhs) $(symbol) $(showExpr eq.rhs)"
    when eq.mod is
        Mod8 -> "$(e) mod 8"
        Standard -> "$(e)"

map : Equation, (Expr -> Expr) -> Equation
map = \eq, f ->
    { eq &
        lhs: f eq.lhs,
        rhs: f eq.rhs,
    }

# expr

Expr : [
    Register Register,
    Num U64,
    Pow2 Expr,
    Log2 Expr,
    Mod8 Expr,
    Mul Expr Expr,
    Div Expr Expr,
    Xor Expr Expr,
    Inc Expr,
    Dec Expr,
]

showExpr : Expr -> Str
showExpr = \expr ->
    when expr is
        Register A -> "A"
        Register B -> "B"
        Register C -> "C"
        Num n -> Num.toStr n
        Xor lhs rhs -> "($(showExpr lhs) ⊕ $(showExpr rhs))"
        Mod8 lhs -> "($(showExpr lhs) % 8)"
        Mul lhs rhs -> "($(showExpr lhs) * $(showExpr rhs))"
        Inc e -> "($(showExpr e) + 1)"
        Dec e -> "($(showExpr e) - 1)"

register : Register -> Expr
register = \r -> Register r

num : U64 -> Expr
num = \n -> Num n

xor : Expr, Expr -> Expr
xor = \c1, c2 ->
    when (c1, c2) is
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

div : Expr, Expr -> Expr
div = \lhs, rhs ->
    when (lhs, rhs) is
        (Num 0, _) -> num 0
        (Num a, Num b) -> num (a // b)
        (Div e (Num a), Num b) -> div e (num (a * b))
        _ -> Div lhs rhs

mul : Expr, Expr -> Expr
mul = \lhs, rhs ->
    when (lhs, rhs) is
        (Num 0, _) | (_, Num 0) -> num 0
        (Num l, Num r) -> num (l * r)
        _ -> Mul lhs rhs

pow2 : Expr -> Expr
pow2 = \expr ->
    when expr is
        Num n -> num (2 |> Num.powInt n)
        _ -> Pow2 expr

log2 : Expr -> Expr
log2 = \expr ->
    when expr is
        Num n -> num (numLog2 n)
        _ -> Log2 expr

mod8 : Expr -> Expr
mod8 = \expr ->
    when expr is
        Num a -> num (a % 8)
        _ -> Mod8 expr

inc : Expr -> Expr
inc = \expr ->
    when expr is
        Num n -> num (n + 1)
        _ -> Inc expr

dec : Expr -> Expr
dec = \expr ->
    when expr is
        Num n -> num (n - 1)
        _ -> Dec expr

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

    _ =
        when op is
            Ok Adv ->
                dbg (memory.a, combo operand)
                {}

            _ ->
                dbg (memory.a, op)
                {}

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

subst : Expr, Register, Expr -> Expr
subst = \expr, r, with ->
    when expr is
        Register r2 if r2 == r -> with
        Register _ -> expr
        Num _ -> expr
        Xor lhs rhs -> xor (subst lhs r with) (subst rhs r with)
        Pow2 i -> pow2 (subst i r with)
        Log2 x -> log2 (subst x r with)
        Mod8 lhs -> mod8 (subst lhs r with)
        Div lhs rhs -> div (subst lhs r with) (subst rhs r with)
        Mul lhs rhs -> mul (subst lhs r with) (subst rhs r with)
        Inc e -> inc (subst e r with)
        Dec e -> dec (subst e r with)

generateEqs : List U64, List Equation, List U64, U64 -> Result (List Equation) [NoSolution]
generateEqs = \program, eqs, output, ip ->
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
            Adv -> eqs |> substAll A (div (register A) (pow2 (combo op)))
            Bdv -> eqs |> substAll B (div (register A) (pow2 (combo op)))
            Cdv -> eqs |> substAll C (div (register A) (pow2 (combo op)))
            Bxl -> eqs |> substAll B (xor (register B) (num op))
            Bxc -> eqs |> substAll B (xor (register B) (register C))
            Bst -> eqs |> substAll B (mod8 (combo op))
            Out ->
                eqs
                |> List.append { lhs: combo op, rhs: num nextOut, type: Equals, mod: Mod8 }

            Jnz -> crash "should not encounter"

    printed = inst == Out
    newOutput = if printed then output |> List.prepend nextOut else output

    if List.len newOutput == List.len program && ip == 0 then
        Ok newEqs
    else
        newIp = if ip == 0 then List.len program - 4 else ip - 2
        generateEqs program newEqs newOutput newIp

substAll : List Equation, Register, Expr -> List Equation
substAll = \eqs, r, with ->
    eqs
    |> List.map \eq ->
        eq
        |> map \expr ->
            subst expr r with

part2 = \input ->
    (memory, program) = parse input

    init = [{ lhs: register A, rhs: num 0, type: Equals, mod: Standard }]
    eqsResult =
        generateEqs program init [] (List.len program - 4)

    _ =
        eqsResult
        |> Result.map \eqs ->
            eqs
            |> List.map \eq ->
                eq
                |> map \expr ->
                    expr
                    |> subst B (num memory.b)
                    |> subst C (num memory.c)
            |> List.map \eq ->
                dbg (showEq eq)
                {}
    0

flip : Equation -> Equation
flip = \eq ->
    { eq &
        lhs: eq.rhs,
        rhs: eq.lhs,
        type:
        when eq.type is
            Equals -> Equals
            Lt -> Gt
            Gt -> Lt
            Gte -> Lte
            Lte -> Gte,
    }

simplify : Expr -> Expr
simplify = \expr -> expr
# when expr is
#    Register _ -> expr
#    Mod8 (Div a b) ->
