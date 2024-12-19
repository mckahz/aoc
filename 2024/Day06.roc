module [example, myInput, part1, part2]

import "inputs/day06-example.txt" as example : List U8
import "inputs/day06-input.txt" as myInput : List U8

import Linear
import Map exposing [Pos, Dir, cw90]

Map : Map.Map [Guard, Obstacle]

Guard : {
    pos : Pos,
    dir : Dir,
}

parse = \input ->
    map =
        input
        |> Map.parse \c ->
            when c is
                '#' -> Cell Obstacle
                _ -> Empty

    guardPos =
        input
        |> Map.parse \c ->
            when c is
                '^' -> Cell Guard
                _ -> Empty
        |> Map.find Guard
        |> Result.withDefault Linear.zero

    ({ pos: guardPos, dir: Up }, map)

getOffset : Dir -> Linear.V2
getOffset = \dir ->
    when dir is
        Up -> { x: 0, y: -1 }
        Right -> { x: 1, y: 0 }
        Down -> { x: 0, y: 1 }
        Left -> { x: -1, y: 0 }

inside : Guard, Map -> Bool
inside = \guard, map ->
    { x, y } = guard.pos
    (0 <= x && x <= Map.maxX map)
    && (0 <= y && y <= Map.maxY map)

step : Guard, Map -> Guard
step = \guard, map ->
    { x: ox, y: oy } = getOffset guard.dir
    npos = { x: guard.pos.x + ox, y: guard.pos.y + oy }
    if map |> Map.has Obstacle npos then
        { guard & dir: cw90 guard.dir }
    else
        { guard & pos: npos }

simulate : Guard, Map -> Set Pos
simulate = \guard, map ->
    simulateHelp guard map (Set.empty {})

simulateHelp : Guard, Map, Set Pos -> Set Pos
simulateHelp = \guard, map, visited ->
    newGuard = step guard map
    if inside newGuard map then
        simulateHelp newGuard map (visited |> Set.insert guard.pos)
    else
        (visited |> Set.insert guard.pos)

part1 = \input ->
    (guard, map) = parse input
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
    (guard, map) = parse input

    simulate guard map
    |> Set.toList
    |> List.mapWithIndex \p, i -> (p, i)
    |> List.countIf \(position, i) ->
        loops guard (map |> Map.add Obstacle position)
