module [example1, example2, example3, example4, example5, myInput, part1, part2]

import "inputs/day12-example1.txt" as example1 : List U8
import "inputs/day12-example2.txt" as example2 : List U8
import "inputs/day12-example3.txt" as example3 : List U8
import "inputs/day12-example4.txt" as example4 : List U8
import "inputs/day12-example5.txt" as example5 : List U8
import "inputs/day12-input.txt" as myInput : List U8

import Linear exposing [left, right, up, down]
import Map exposing [Pos]

Map : Map.Map U8

Region : Set Pos

parse : List U8 -> Map
parse = \input ->
    input |> Map.parse Cell

appraiseRegions : Set Pos, (Region -> U64) -> U64
appraiseRegions = \positions, eval ->
    appraiseRegionsHelp positions eval 0

appraiseRegionsHelp : Set Pos, (Region -> U64), U64 -> U64
appraiseRegionsHelp = \positions, eval, cost ->
    when positions |> Set.toList is
        [] -> cost
        [position, ..] ->
            startRegion = Set.single position
            region = findRegion positions startRegion
            appraiseRegionsHelp
                (positions |> Set.difference region)
                eval
                (cost + eval region)

findRegion : Set Pos, Region -> Region
findRegion = \positions, region ->
    adj =
        region
        |> Set.joinMap \pos ->
            [left pos, right pos, up pos, down pos]
            |> List.dropIf \p -> region |> Set.contains p
            |> List.keepIf \p -> positions |> Set.contains p
            |> Set.fromList
    if Set.isEmpty adj then
        region
    else
        findRegion
            (positions |> Set.difference adj)
            (region |> Set.union adj)

area : Region -> U64
area = Set.len

perimeter : Region -> U64
perimeter = \region ->
    region
    |> Set.toList
    |> List.map \pos ->
        [left pos, right pos, up pos, down pos]
        |> List.countIf \adjPos -> !(region |> Set.contains adjPos)
    |> List.sum

part1 = \input ->
    map = parse input
    appraiseMap map (\region -> area region * perimeter region)

Dir : [Left, Right, Up, Down]

Edge : {
    min : I32,
    max : I32,
    at : I32,
    dir : Dir,
}

count : List Edge -> U64
count = \edges -> [
        edges |> countFacing Left,
        edges |> countFacing Right,
        edges |> countFacing Up,
        edges |> countFacing Down,
    ]
    |> List.sum

countFacing : List Edge, Dir -> U64
countFacing = \edges, dir ->
    alignedEdges =
        edges
        |> List.keepIf \edge -> dir == edge.dir

    min =
        alignedEdges
        |> List.map .at
        |> List.min
        |> Result.withDefault 0

    max =
        alignedEdges
        |> List.map .at
        |> List.max
        |> Result.withDefault 0

    List.range { start: At min, end: At max }
    |> List.map \at ->
        alignedEdges |> countAt at
    |> List.sum

# this assumes all edges are facing the same direction
countAt : List Edge, I32 -> U64
countAt = \edges, at ->
    edges
    |> List.keepIf \other -> other.at == at
    |> List.sortWith \e1, e2 -> Num.compare e1.min e2.min
    |> List.walk [] \ls, e ->
        when ls is
            [.. as init, last] if last.max + 1 == e.min ->
                init |> List.append { last & max: e.max }

            _ ->
                ls |> List.append e
    |> List.len

findEdges : Region -> List Edge
findEdges = \region ->
    region
    |> Set.toList
    |> List.joinMap \pos ->
        [
            (left pos, Left),
            (right pos, Right),
            (up pos, Up),
            (down pos, Down),
        ]
        |> List.keepOks \(adjPos, dir) ->
            if region |> Set.contains adjPos then
                Err NotAnEdge
            else
                when dir is
                    Left | Right -> Ok { dir, at: pos.x, min: pos.y, max: pos.y }
                    Up | Down -> Ok { dir, at: pos.y, min: pos.x, max: pos.x }

sides : Region -> U64
sides = \region ->
    region
    |> findEdges
    |> count

appraiseMap : Map, (Region -> U64) -> U64
appraiseMap = \map, eval ->
    map.cells
    |> Dict.map \_, positions ->
        appraiseRegions positions eval
    |> Dict.values
    |> List.sum

part2 = \input ->
    map = parse input
    appraiseMap map (\region -> area region * sides region)
