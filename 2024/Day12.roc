module [example1, example2, example3, example4, example5, myInput, part1, part2]

import "inputs/day12-example1.txt" as example1 : List U8
import "inputs/day12-example2.txt" as example2 : List U8
import "inputs/day12-example3.txt" as example3 : List U8
import "inputs/day12-example4.txt" as example4 : List U8
import "inputs/day12-example5.txt" as example5 : List U8
import "inputs/day12-input.txt" as myInput : List U8

Map : Dict U8 (List Pos)

Pos : (I32, I32)

Region : List Pos

left : Pos -> Pos
left = \(x, y) -> (x - 1, y)

right : Pos -> Pos
right = \(x, y) -> (x + 1, y)

up : Pos -> Pos
up = \(x, y) -> (x, y - 1)

down : Pos -> Pos
down = \(x, y) -> (x, y + 1)

parse : List U8 -> Map
parse = \input ->
    input
    |> List.dropLast 1
    |> List.splitOn '\n'
    |> List.mapWithIndex \line, y ->
        line
        |> List.mapWithIndex \plant, x ->
            (plant, (Num.toI32 x, Num.toI32 y))
    |> List.join
    |> List.walk (Dict.empty {}) \map, (plant, pos) ->
        Dict.update map plant \result ->
            when result is
                Err Missing -> Ok [pos]
                Ok poss -> Ok (List.append poss pos)

nextTo : Pos, Pos -> Bool
nextTo = \(x1, y1), (x2, y2) ->
    (x1 == x2 + 1 && y1 == y2)
    || (x1 == x2 - 1 && y1 == y2)
    || (y1 == y2 + 1 && x1 == x2)
    || (y1 == y2 - 1 && x1 == x2)

findRegions : List Pos -> List (List Pos)
findRegions = \positions ->
    positions
    |> List.walk
        { regions: [], searchedPositions: [] }
        \state, position ->
            if state.searchedPositions |> List.contains position then
                state
            else
                region = findRegion positions [position]
                {
                    regions: state.regions |> List.append region,
                    searchedPositions: state.searchedPositions |> List.concat region,
                }
    |> .regions

findRegion : List Pos, List Pos -> List Pos
findRegion = \positions, region ->
    adj =
        positions
        |> List.keepIf \other ->
            region
            |> List.any \p ->
                p |> nextTo other
    newRegion = region |> List.concat adj |> Set.fromList |> Set.toList
    remaining =
        positions |> List.dropIf \p -> newRegion |> List.contains p
    if List.isEmpty adj then
        region
    else
        findRegion remaining newRegion

area : List Pos -> U64
area = List.len

perimeter : List Pos -> U64
perimeter = \positions ->
    positions
    |> List.map \pos ->
        [left pos, right pos, up pos, down pos]
        |> List.countIf \adjPos -> !(positions |> List.contains adjPos)
    |> List.sum

part1 = \input ->
    map = parse input

    map
    |> Dict.map \plant, positions ->
        findRegions positions
        |> List.map \region -> area region * perimeter region
    |> Dict.values
    |> List.map List.sum
    |> List.sum

Dir : [Left, Right, Up, Down]
Edge : {
    min : I32,
    max : I32,
    at : I32,
    dir : Dir,
}

joinLinear : List Edge, Dir, I32 -> List Edge
joinLinear = \edges, dir, at ->
    edges
    |> List.keepIf \other -> other.at == at
    |> List.keepIf \other -> other.dir == dir
    |> List.sortWith \e1, e2 -> Num.compare e1.min e2.min
    |> List.walk [] \ls, e ->
        when ls is
            [.. as init, last] if last.max + 1 == e.min ->
                init |> List.append { last & max: e.max }

            _ ->
                ls |> List.append e

joinOnly : List Edge, Dir -> List Edge
joinOnly = \edges, dir ->
    min =
        edges
        |> List.keepIf \edge -> dir == edge.dir
        |> List.map .at
        |> List.min
        |> Result.withDefault 0

    max =
        edges
        |> List.keepIf \edge -> dir == edge.dir
        |> List.map .at
        |> List.max
        |> Result.withDefault 0

    List.range { start: At min, end: At max }
    |> List.joinMap \at ->
        joinLinear edges dir at

join : List Edge -> List Edge
join = \edges -> [
        joinOnly edges Left,
        joinOnly edges Right,
        joinOnly edges Up,
        joinOnly edges Down,
    ]
    |> List.join

findEdges : List Pos -> List Edge
findEdges = \positions ->
    pos <- positions |> List.joinMap
    [
        (left pos, Left),
        (right pos, Right),
        (up pos, Up),
        (down pos, Down),
    ]
    |> List.keepOks \(adjPos, dir) ->
        if !(positions |> List.contains adjPos) then
            when dir is
                Left | Right -> Ok { dir, at: pos.0, min: pos.1, max: pos.1 }
                Up | Down -> Ok { dir, at: pos.1, min: pos.0, max: pos.0 }
        else
            Err NotAnEdge

sides : List Pos -> U64
sides = \positions ->
    positions
    |> findEdges
    |> join
    |> List.len

part2 = \input ->
    map = parse input
    map
    |> Dict.map \plant, positions ->
        findRegions positions
        |> List.map \region -> area region * sides region
    |> Dict.values
    |> List.map List.sum
    |> List.sum
