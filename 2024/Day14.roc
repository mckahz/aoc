module [example, myInput, part1, part2, show]

import "inputs/day14-example.txt" as example : List U8
import "inputs/day14-input.txt" as myInput : List U8

import parser.Parser as P
import parser.String as PS

Vec : {
    x : I32,
    y : I32,
}

Ray : {
    pos : Vec,
    vel : Vec,
}

parse = \input ->
    num =
        P.const \sign -> \n ->
                sign * Num.toI32 n
        |> P.keep
            (
                PS.oneOf [
                    P.const -1 |> P.skip (PS.codeunit '-'),
                    P.const 1,
                ]
            )
        |> P.keep PS.digits

    rays =
        P.const \px -> \py -> \vx -> \vy ->
                        { pos: { x: px, y: py }, vel: { x: vx, y: vy } }
        |> P.skip (PS.string "p=")
        |> P.keep num
        |> P.skip (PS.string ",")
        |> P.keep num
        |> P.skip (PS.string " v=")
        |> P.keep num
        |> P.skip (PS.string ",")
        |> P.keep num
        |> P.sepBy (PS.codeunit '\n')

    result = P.parse rays input (\_ -> Bool.true)

    when result is
        Ok r -> r
        Err e ->
            dbg e
            crash "invalid input"

step : Ray, U64, Vec -> Vec
step = \{ pos, vel }, n, dims ->
    x = (pos.x + vel.x * Num.toI32 n) % dims.x
    y = (pos.y + vel.y * Num.toI32 n) % dims.y
    {
        x: if x < 0 then x + dims.x else x,
        y: if y < 0 then y + dims.y else y,
    }

split : List Vec, Vec -> List (List Vec)
split = \positions, dims ->
    hx = dims.x // 2
    hy = dims.y // 2
    [
        positions |> List.keepIf \{ x, y } -> x < hx && y < hy,
        positions |> List.keepIf \{ x, y } -> x > hx && y < hy,
        positions |> List.keepIf \{ x, y } -> x < hx && y > hy,
        positions |> List.keepIf \{ x, y } -> x > hx && y > hy,
    ]

part1 = \input, dims ->
    robots = parse input
    robots
    |> List.map \robot -> step robot 100 dims
    |> split dims
    |> List.map List.len
    |> List.product

show : List Vec, Vec -> Str
show = \robots, dims ->
    List.range { start: At 0, end: Length (Num.toU64 dims.y) }
    |> List.map \y ->
        List.range { start: At 0, end: Length (Num.toU64 dims.x) }
        |> List.map \x ->
            if robots |> List.any \robot -> robot.x == Num.toI32 x && robot.y == Num.toI32 y then
                "#"
            else
                " "
        |> Str.joinWith ""
    |> List.prepend ""
    |> Str.joinWith "\n"

part2 = \_input, _dims ->
    iter = 8159
    # robots = parse input
    # tree =
    #    robots
    #    |> List.map \robot -> step robot iter dims
    #    |> show dims
    # dbg tree
    iter
