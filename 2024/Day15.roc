module [example1, example2, myInput, part1, part2]

import "inputs/day15-example1.txt" as example1 : List U8
import "inputs/day15-example2.txt" as example2 : List U8
import "inputs/day15-input.txt" as myInput : List U8

Pos : {
    x : I32,
    y : I32,
}

Map : {
    walls : Set Pos,
    rocks : Set Pos,
    robot : Pos,
}

WideMap : {
    walls : Set Pos,
    rocks : Set Pos,
    robot : Pos,
    wide : {},
}

Dir : [Left, Right, Up, Down]

left : Pos -> Pos
left = \p -> { p & x: p.x - 1 }

right : Pos -> Pos
right = \p -> { p & x: p.x + 1 }

up : Pos -> Pos
up = \p -> { p & y: p.y - 1 }

down : Pos -> Pos
down = \p -> { p & y: p.y + 1 }

parse : List U8 -> (Map, List Dir)
parse = \input ->
    list = input |> List.splitOnList ['\n', '\n']
    (mapStr, dirStr) =
        when list is
            [m, d] -> (m, d)
            _ -> crash "invalid input"

    positions =
        mapStr
        |> List.splitOn '\n'
        |> List.mapWithIndex \row, y ->
            row
            |> List.mapWithIndex \c, x ->
                type =
                    when c is
                        '#' -> Wall
                        'O' -> Rock
                        '@' -> Robot
                        _ -> Nothing
                (type, { x: Num.toI32 x, y: Num.toI32 y })
        |> List.join

    walls =
        positions
        |> List.keepOks \(type, pos) ->
            if type == Wall then
                Ok pos
            else
                Err {}
        |> Set.fromList

    rocks =
        positions
        |> List.keepOks \(type, pos) ->
            if type == Rock then
                Ok pos
            else
                Err {}
        |> Set.fromList

    robot =
        positions
        |> List.findFirst \(type, _) -> type == Robot
        |> Result.map .1
        |> Result.withDefault { x: 0, y: 0 }

    directions =
        dirStr
        |> List.keepOks \c ->
            when c is
                '<' -> Ok Left
                '>' -> Ok Right
                '^' -> Ok Up
                'v' -> Ok Down
                _ -> Err {}

    ({ walls, rocks, robot }, directions)

move : Pos, Dir -> Pos
move = \pos, dir ->
    f =
        when dir is
            Left -> left
            Right -> right
            Up -> up
            Down -> down
    f pos

show : Map -> Str
show = \map ->
    maxx =
        map.walls
        |> Set.map \{ x } -> x
        |> Set.toList
        |> List.max
        |> Result.withDefault 0
        |> Num.toU64

    maxy =
        map.walls
        |> Set.map \{ y } -> y
        |> Set.toList
        |> List.max
        |> Result.withDefault 0
        |> Num.toU64

    List.range { start: At 0, end: At maxy }
    |> List.map Num.toI32
    |> List.map \y ->
        List.range { start: At 0, end: At maxx }
        |> List.map Num.toI32
        |> List.map \x ->
            if map.walls |> Set.contains { x, y } then
                "#"
            else if map.rocks |> Set.contains { x, y } then
                "O"
            else if map.robot == { x, y } then
                "@"
            else
                " "
        |> Str.joinWith ""
    |> List.prepend ""
    |> Str.joinWith "\n"

step : Map, Dir -> Map
step = \map, dir ->
    robot = move map.robot dir
    moveRocks map robot dir
    |> Result.map \m -> { m & robot: if m.walls |> Set.contains robot then m.robot else robot }
    |> Result.withDefault map

moveRocks : Map, Pos, Dir -> Result Map [CannotMove]
moveRocks = \map, from, dir ->
    to = move from dir
    moveRock = \m -> { m & rocks: m.rocks |> Set.remove from |> Set.insert to }
    if !(map.rocks |> Set.contains from) then
        Ok map
    else if map.walls |> Set.contains to then
        Err CannotMove
    else if map.rocks |> Set.contains to then
        newMap = moveRocks? map to dir
        Ok (moveRock newMap)
    else
        Ok (moveRock map)

score : Map -> I32
score = \map ->
    map.rocks
    |> Set.toList
    |> List.map \{ x, y } -> 100 * y + x
    |> List.sum

part1 = \input ->
    (map, dirs) = parse input
    dirs
    |> List.walkWithIndex map \m, dir, i -> step m dir
    |> score

grow : Map -> WideMap
grow = \map ->
    widen = \{ x, y } -> { x: 2 * x, y }
    {
        rocks: map.rocks |> Set.map widen,
        walls: map.walls |> Set.map widen,
        robot: widen map.robot,
        wide: {},
    }

showWide : WideMap -> Str
showWide = \map ->
    maxx =
        map.walls
        |> Set.map \{ x } -> x
        |> Set.toList
        |> List.max
        |> Result.withDefault 0
        |> Num.toU64
        |> Num.add 1

    maxy =
        map.walls
        |> Set.map \{ y } -> y
        |> Set.toList
        |> List.max
        |> Result.withDefault 0
        |> Num.toU64
        |> Num.add 1

    List.range { start: At 0, end: At maxy }
    |> List.map Num.toI32
    |> List.map \y ->
        List.range { start: At 0, end: At maxx }
        |> List.map Num.toI32
        |> List.map \x ->
            wallHere = map.walls |> Set.contains { x, y }
            wallLeft = map.walls |> Set.contains { x: x - 1, y }
            rockHere = map.rocks |> Set.contains { x, y }
            rockLeft = map.rocks |> Set.contains { x: x - 1, y }
            robotLeft = map.robot == { x: x - 1, y }
            if wallLeft then
                "#"
            else if wallHere then
                "#"
            else if rockLeft then
                "]"
            else if rockHere then
                "["
            else if robotLeft then
                " "
            else if map.robot == { x, y } then
                "@"
            else
                " "
        |> Str.joinWith ""
    |> List.prepend ""
    |> Str.joinWith "\n"

stepWide : WideMap, Dir -> WideMap
stepWide = \map, dir ->
    robot = move map.robot dir
    robotLeft = { robot & x: robot.x - 1 }
    wallInWay = (map.walls |> Set.contains robot) || (map.walls |> Set.contains { robot & x: robot.x - 1 })
    if wallInWay then
        map
    else if map.rocks |> Set.contains robot then
        moveRocksWide map robot dir
        |> Result.map \m -> { m & robot }
        |> Result.withDefault map
    else if map.rocks |> Set.contains robotLeft then
        moveRocksWide map robotLeft dir
        |> Result.map \m -> { m & robot }
        |> Result.withDefault map
    else
        { map & robot }

moveRocksWide : WideMap, Pos, Dir -> Result WideMap [CannotMove]
moveRocksWide = \map, pos, dir ->
    newPos = move pos dir
    leftNewPos = move { pos & x: pos.x - 1 } dir
    rightNewPos = move { pos & x: pos.x + 1 } dir
    moveRock = \m -> { m & rocks: m.rocks |> Set.remove pos |> Set.insert newPos }
    if !(map.rocks |> Set.contains pos) then
        Ok map
    else if map.walls |> Set.contains newPos then
        Err CannotMove
    else if map.walls |> Set.contains leftNewPos then
        Err CannotMove
    else if map.walls |> Set.contains rightNewPos then
        Err CannotMove
    else
        [
            Ok newPos,
            if dir == Right then Err {} else Ok leftNewPos,
            if dir == Left then Err {} else Ok rightNewPos,
        ]
        |> List.keepOks \p -> p
        |> List.keepIf \p -> map.rocks |> Set.contains p
        |> List.walkTry map \m, p ->
            moveRocksWide m p dir
        |> Result.map moveRock

scoreWide : WideMap -> I32
scoreWide = \map -> score { walls: map.walls, rocks: map.rocks, robot: map.robot }

part2 = \input ->
    (map, dirs) = parse input
    dirs
    |> List.walkWithIndex (grow map) \m, dir, i ->
        stepWide m dir
    |> scoreWide
