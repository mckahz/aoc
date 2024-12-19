module [
    Map,
    parse,
    findAll,
    find,
    has,
    get,
    show,
    mapPos,
    maxX,
    maxY,
    add,
    remove,
    removeAt,
    getAllCells,
    Pos,
    move,
    Dir,
    cw90,
]

import Linear

Pos : Linear.V2

Map cell : {
    cells : Dict cell (Set Pos),
}

Dir : [Up, Left, Down, Right]

getAllCells : Map cell -> Set cell
getAllCells = \map ->
    map.cells
    |> Dict.keys
    |> Set.fromList

cw90 : Dir -> Dir
cw90 = \dir ->
    when dir is
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up

get : Map cell, Pos -> Result cell [EmptyCell]
get = \map, pos ->
    map.cells
    |> Dict.toList
    |> List.findFirst \(cell, positions) ->
        positions |> Set.contains pos
    |> Result.map .0
    |> Result.mapErr \_ -> EmptyCell

mapPos : Map cell, (Pos -> Pos) -> Map cell
mapPos = \map, f -> {
    cells: map.cells
    |> Dict.map \cell, positions ->
        positions
        |> Set.map f,
}

move : Pos, Dir -> Pos
move = \pos, dir ->
    f =
        when dir is
            Left -> Linear.left
            Right -> Linear.right
            Up -> Linear.up
            Down -> Linear.down
    f pos

parse : List U8, (U8 -> [Cell cell, Empty]) -> Map cell
parse = \input, getCell ->
    input
    |> List.splitOn '\n'
    |> List.mapWithIndex \row, y ->
        row
        |> List.mapWithIndex \c, x ->
            pos = { x: Num.toI32 x, y: Num.toI32 y }
            (getCell c, pos)
    |> List.join
    |> List.keepOks \(cell, pos) ->
        when cell is
            Cell c -> Ok (c, pos)
            Empty -> Err {}
    |> List.walk (Dict.empty {}) \cells, (cell, pos) ->
        Dict.update cells cell \result ->
            when result is
                Ok positions -> Ok (positions |> Set.insert pos)
                Err Missing -> Ok (Set.single pos)
    |> \cells -> { cells }

remove : Map cell, cell -> Map cell
remove = \map, cell -> {
    cells: map.cells
    |> Dict.remove cell,
}

removeAt : Map cell, Pos -> Map cell
removeAt = \map, pos -> {
    cells: map.cells
    |> Dict.map \_, positions ->
        positions |> Set.remove pos,
}

add : Map cell, cell, Pos -> Map cell
add = \map, cell, pos -> {
    cells: map.cells
    |> Dict.update cell \result ->
        when result is
            Err Missing -> Ok (Set.single pos)
            Ok positions -> Ok (positions |> Set.insert pos),
}

has : Map cell, cell, Pos -> Bool
has = \map, cell, pos ->
    map
    |> findAll cell
    |> Set.contains pos

findAll : Map cell, cell -> Set Pos
findAll = \map, cell ->
    map.cells
    |> Dict.get cell
    |> Result.withDefault (Set.empty {})

find : Map cell, cell -> Result Pos [CellNotInMap]
find = \map, cell ->
    findAll map cell
    |> Set.toList
    |> List.first
    |> Result.mapErr \_ -> CellNotInMap

maxX : Map cell -> I32
maxX = \map ->
    map.cells
    |> Dict.values
    |> Set.fromList
    |> Set.joinMap \ps -> ps
    |> Set.map \{ x } -> x
    |> Set.toList
    |> List.max
    |> Result.withDefault 0

maxY : Map cell -> I32
maxY = \map ->
    map.cells
    |> Dict.values
    |> Set.fromList
    |> Set.joinMap \ps -> ps
    |> Set.map \{ y } -> y
    |> Set.toList
    |> List.max
    |> Result.withDefault 0

show : Map cell, ([Cell cell, Empty] -> Str) -> Str
show = \map, showCell ->
    maxx = maxX map |> Num.toU64
    maxy = maxY map |> Num.toU64

    List.range { start: At 0, end: At maxy }
    |> List.map \y ->
        List.range { start: At 0, end: At maxx }
        |> List.map \x ->
            map
            |> get { x: Num.toI32 x, y: Num.toI32 y }
            |> Result.map Cell
            |> Result.withDefault Empty
            |> showCell
        |> Str.joinWith ""
    |> List.prepend ""
    |> Str.joinWith "\n"
