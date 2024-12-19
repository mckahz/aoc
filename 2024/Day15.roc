module [example1, example2, myInput, part1, part2]

import "inputs/day15-example1.txt" as example1 : List U8
import "inputs/day15-example2.txt" as example2 : List U8
import "inputs/day15-input.txt" as myInput : List U8
import Linear
import Map exposing [Pos, Dir, move]

Cell : [Robot, Rock, Wall]
WideCell : [Robot, Rock, Wall]
Map : Map.Map Cell
WideMap : Map.Map WideCell

parse : List U8 -> (Map, List Dir)
parse = \input ->
    list = input |> List.splitOnList ['\n', '\n']
    (mapStr, dirStr) =
        when list is
            [m, d] -> (m, d)
            _ -> crash "invalid input"

    map =
        mapStr
        |> Map.parse \c ->
            when c is
                '#' -> Cell Wall
                'O' -> Cell Rock
                '@' -> Cell Robot
                _ -> Empty

    directions =
        dirStr
        |> List.keepOks \c ->
            when c is
                '<' -> Ok Left
                '>' -> Ok Right
                '^' -> Ok Up
                'v' -> Ok Down
                _ -> Err {}

    (map, directions)

show : Map -> Str
show = \map ->
    map
    |> Map.show \c ->
        when c is
            Cell Wall -> "#"
            Cell Rock -> "O"
            Cell Robot -> "@"
            Empty -> " "

step : Map, Dir -> Map
step = \map, dir ->
    oldRobotPos = map |> Map.find Robot |> Result.withDefault Linear.zero
    newRobotPos = move oldRobotPos dir
    moveRocks map newRobotPos dir
    |> Result.map \m -> m |> Map.remove Robot |> Map.add Robot newRobotPos
    |> Result.withDefault map

moveRocks : Map, Pos, Dir -> Result Map [CannotMove]
moveRocks = \map, from, dir ->
    to = move from dir
    moveRock = \m -> m |> Map.removeAt from |> Map.add Rock to
    if map |> Map.has Wall from then
        Err CannotMove
    else if !(map |> Map.has Rock from) then
        Ok map
    else if map |> Map.has Wall to then
        Err CannotMove
    else if map |> Map.has Rock to then
        newMap = moveRocks? map to dir
        Ok (moveRock newMap)
    else
        Ok (moveRock map)

score : Map -> I32
score = \map ->
    map
    |> Map.findAll Rock
    |> Set.toList
    |> List.map \{ x, y } -> 100 * y + x
    |> List.sum

part1 = \input ->
    (map, dirs) = parse input
    dirs
    |> List.walk map \m, dir -> step m dir
    |> score
    |> dbg

grow : Map -> WideMap
grow = \map ->
    map |> Map.mapPos \{ x, y } -> { x: 2 * x, y }

showWide : WideMap -> Str
showWide = \map ->
    maxx = Map.maxX map
    maxy = Map.maxY map

    List.range { start: At 0, end: At maxy }
    |> List.map Num.toI32
    |> List.map \y ->
        List.range { start: At 0, end: At maxx }
        |> List.map Num.toI32
        |> List.map \x ->
            here = { x, y }
            left = { x: x - 1, y }
            if map |> Map.has Wall here then
                "#"
            else if map |> Map.has Wall left then
                "#"
            else if map |> Map.has Rock left then
                "]"
            else if map |> Map.has Rock here then
                "["
            else if map |> Map.has Robot left then
                " "
            else if map |> Map.has Robot here then
                "@"
            else
                " "
        |> Str.joinWith ""
    |> List.prepend ""
    |> Str.joinWith "\n"

stepWide : WideMap, Dir -> WideMap
stepWide = \map, dir ->
    oldRobotPos = map |> Map.find Robot |> Result.withDefault Linear.zero
    robotPos = move oldRobotPos dir
    leftRobotPos = { robotPos & x: robotPos.x - 1 }
    moveRobot = \m -> m |> Map.remove Robot |> Map.add Robot robotPos
    wallInWay = (map |> Map.has Wall robotPos) || (map |> Map.has Wall leftRobotPos)
    if wallInWay then
        map
    else if map |> Map.has Rock robotPos then
        moveRocksWide map robotPos dir
        |> Result.map moveRobot
        |> Result.withDefault map
    else if map |> Map.has Rock leftRobotPos then
        moveRocksWide map leftRobotPos dir
        |> Result.map moveRobot
        |> Result.withDefault map
    else
        map
        |> Map.remove Robot
        |> Map.add Robot robotPos

moveRocksWide : WideMap, Pos, Dir -> Result WideMap [CannotMove]
moveRocksWide = \map, pos, dir ->
    newPos = move pos dir
    leftNewPos = { newPos & x: newPos.x - 1 }
    rightNewPos = { newPos & x: newPos.x + 1 }

    if !(map |> Map.has Rock pos) then
        Ok map
    else if map |> Map.has Wall newPos then
        Err CannotMove
    else if map |> Map.has Wall leftNewPos then
        Err CannotMove
    else if map |> Map.has Wall rightNewPos then
        Err CannotMove
    else
        nextRockPositions =
            [
                Ok newPos,
                if dir == Right then Err {} else Ok leftNewPos,
                if dir == Left then Err {} else Ok rightNewPos,
            ]
            |> List.keepOks \p -> p
            |> List.keepIf \p -> (map |> Map.has Rock p)

        nextRockPositions
        |> List.walkTry map \m, p ->
            moveRocksWide m p dir
        |> Result.map \m -> m |> Map.removeAt pos |> Map.add Rock newPos

part2 = \input ->
    (map, dirs) = parse input
    dirs
    |> List.walk (grow map) \m, dir ->
        stepWide m dir
    |> score
