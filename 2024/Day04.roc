module [example, myInput, part1, part2]

import "inputs/day04-example.txt" as example : List U8
import "inputs/day04-input.txt" as myInput : List U8

# a more complete API would offer
# findIndices : Grid, (U8 -> Bool) -> List U64
# which is evidently useful from part 2

Coord : (I64, I64)
Grid : {
    rows : List (List U8),
    mapping : Coord -> Coord,
    angle : [Slanted, Straight],
}

parse : List U8 -> Grid
parse = \input -> {
    rows: input
    |> List.dropLast 1
    |> List.splitOn '\n',
    mapping: \x -> x,
    angle: Straight,
}

render : Grid -> Str
render = \grid ->
    width = getWidth grid |> Num.toI64
    height = getHeight grid |> Num.toI64
    List.range { start: At 0, end: Length (Num.toU64 ((width + 1) * height - 1)) }
    |> List.keepOks \i ->
        x = i % (width + 1)
        y = i // (width + 1)
        if x == width then
            Ok '\n'
        else
            get grid (x, y)
    |> Str.fromUtf8
    |> Result.withDefault ""

get : Grid, Coord -> Result U8 [OutOfBounds]
get = \grid, coord ->
    (x, y) = grid.mapping coord
    (grid.rows |> List.get (Num.toU64 y))?
    |> List.get (Num.toU64 x)

getWidth : Grid -> U64
getWidth = \grid ->
    width =
        grid.rows
        |> List.map List.len
        |> List.max
        |> Result.withDefault 0

    when grid.angle is
        Straight -> width
        Slanted -> 2 * width

getHeight : Grid -> U64
getHeight = \grid ->
    height = List.len grid.rows

    when grid.angle is
        Straight -> height
        Slanted -> 2 * height

rotate : Grid, U64 -> Grid
rotate = \grid, angle ->
    List.concat
        (List.repeat cw90 (angle // 90))
        (List.repeat cw45 ((angle % 90) // 45))
    |> List.walk grid \g, t -> t g

cw45 : Grid -> Grid
cw45 = \grid ->
    mapping = grid.mapping

    { grid &
        mapping: \(cx, cy) -> mapping (cx, cy - cx),
        angle:
        when grid.angle is
            Slanted -> Straight
            Straight -> Slanted,
    }

cw90 : Grid -> Grid
cw90 = \grid ->
    height = getHeight grid |> Num.toI64
    mapping = grid.mapping

    { grid &
        mapping: \(cx, cy) -> mapping (cy, height - 1 - cx),
    }

countXmasL2R : Grid -> U64
countXmasL2R = \grid ->
    width = getWidth grid
    height = getHeight grid

    List.range { start: At 0, end: Length height }
    |> List.map \r ->
        row =
            List.range { start: At 0, end: Length width }
            |> List.keepOks \col -> get grid (col, r)
        List.map4
            row
            (row |> List.dropFirst 1)
            (row |> List.dropFirst 2)
            (row |> List.dropFirst 3)
            \x, m, a, s ->
                (x, m, a, s) == ('X', 'M', 'A', 'S')
        |> List.countIf \x -> x
    |> List.sum

part1 = \input ->
    grid = parse input
    [0, 45, 90, 135, 180, 225, 270, 315]
    |> List.map \angle ->
        grid
        |> rotate angle
        |> countXmasL2R
    |> List.sum

part2 = \input ->
    grid = parse input
    width = getWidth grid |> Num.toI64
    height = getHeight grid |> Num.toI64

    aCoords =
        grid.rows
        |> List.mapWithIndex \row, y ->
            row
            |> List.mapWithIndex \c, x ->
                if c == 'A' then
                    [(Num.toI64 x, Num.toI64 y)]
                else
                    []
            |> List.join
        |> List.join

    isCenterOfCross = \(x, y) ->
        [
            ((-1, -1), (1, 1)),
            ((1, -1), (-1, 1)),
        ]
        |> List.all \((o1x, o1y), (o2x, o2y)) ->
            p1 = get grid (x + o1x, y + o1y)
            p2 = get grid (x + o2x, y + o2y)
            (p1 == Ok 'M' && p2 == Ok 'S')
            || (p1 == Ok 'S' && p2 == Ok 'M')

    aCoords |> List.countIf isCenterOfCross
