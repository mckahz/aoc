module [example, myInput, part1, part2]

import "inputs/day06-example.txt" as example : List U8
import "inputs/day06-input.txt" as myInput : List U8

Dir : [Up, Down, Left, Right]
Pos : (I32, I32)

Guard : {
    position : Pos,
    direction : Dir,
}
Map : {
    obstacles : Set Pos,
    width : I32,
    height : I32,
}

parse = \input ->
    lines =
        input
        |> List.splitOn '\n'
        |> List.dropLast 1

    width = lines |> List.map List.len |> List.max |> Result.withDefault 0 |> Num.toI32
    height = lines |> List.len |> Num.toI32

    positions =
        lines
        |> List.mapWithIndex \line, y ->
            line
            |> List.mapWithIndex \c, x -> (c, x)
            |> List.keepOks \(c, x) ->
                when c is
                    '^' -> Ok (Guard, (Num.toI32 x, Num.toI32 y))
                    '#' -> Ok (Obstacle, (Num.toI32 x, Num.toI32 y))
                    _ -> Err {}
        |> List.join

    obstacles =
        positions
        |> List.keepOks \(t, pos) ->
            when t is
                Obstacle -> Ok pos
                _ -> Err {}
        |> Set.fromList

    guard = {
        position: positions
        |> List.keepOks \(t, pos) ->
            when t is
                Guard -> Ok pos
                _ -> Err {}
        |> List.first
        |> Result.withDefault (0, 0),
        direction: Up,
    }

    { guard, map: { obstacles, width, height } }

cw90 : Dir -> Dir
cw90 = \dir ->
    when dir is
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up

getOffset : Dir -> (I32, I32)
getOffset = \dir ->
    when dir is
        Up -> (0, -1)
        Right -> (1, 0)
        Down -> (0, 1)
        Left -> (-1, 0)

inside : Guard, Map -> Bool
inside = \guard, map ->
    (x, y) = guard.position
    (0 <= x && x < map.width)
    && (0 <= y && y < map.height)

step : Guard, Map -> Guard
step = \guard, map ->
    (ox, oy) = getOffset guard.direction
    npos = (guard.position.0 + ox, guard.position.1 + oy)
    if map.obstacles |> Set.contains npos then
        { guard & direction: cw90 guard.direction }
    else
        { guard & position: npos }

simulate : Guard, Map -> Set Pos
simulate = \guard, map ->
    simulateHelp guard map (Set.empty {})

simulateHelp : Guard, Map, Set Pos -> Set Pos
simulateHelp = \guard, map, visited ->
    newGuard = step guard map
    if inside newGuard map then
        simulateHelp newGuard map (visited |> Set.insert guard.position)
    else
        (visited |> Set.insert guard.position)

part1 = \input ->
    { guard, map } = parse input
    simulate guard map
    |> Set.len

loops : Guard, Map -> Bool
loops = \guard, map ->
    loopsHelp guard map (Set.empty {})

loopsHelp : Guard, Map, Set Guard -> Bool
loopsHelp = \guard, map, guardStates ->
    newGuard = step guard map
    if !(inside newGuard map) then
        Bool.false
    else if guardStates |> Set.contains newGuard then
        Bool.true
    else
        loopsHelp newGuard map (guardStates |> Set.insert guard)

part2 = \input ->
    { guard, map } = parse input

    simulate guard map
    |> Set.toList
    |> List.mapWithIndex \p, i -> (p, i)
    |> List.countIf \(position, i) ->
        loops guard { map & obstacles: map.obstacles |> Set.insert position }
