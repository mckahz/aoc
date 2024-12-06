interface PipeGrid
  exposes [PipeGrid, fromStr, findPath]
  imports
    [ Cell.{ Cell }
    , Pipe.{ Pipe }
    ]

PipeGrid : { pipes: Dict Coord Pipe, start: Coord, width: Nat, height: Nat}
Coord : (Nat, Nat)
Offset : (I32, I32)

fromStr : Str -> Result PipeGrid [NoCells, MoreThanOneStart, NoStart, ContainsInvalidCell]
fromStr = \str ->
  lines =
    str
      |> Str.split "\n"
      |> List.dropLast 1

  firstLine <- Result.try (
    lines
      |> List.first
      |> Result.mapErr \_ -> NoCells
  )

  height = List.len lines
  width = Str.countGraphemes firstLine

  graphemes = lines |> List.joinMap Str.graphemes

  cells <- Result.try (
    List.range { start: At 0, end: Before (height * width) }
      |> List.map2 graphemes \i, grapheme -> (i, grapheme)
      |> List.mapTry \(i, grapheme) ->
          Cell.fromStr grapheme
            |> Result.map \cell -> ((i // width, i % width), cell)
            |> Result.mapErr \_ -> ContainsInvalidCell
      |> Result.map Dict.fromList
    )

  starts : List Coord
  starts =
    cells
      |> Dict.keepIf \(_, cell) -> cell == Start
      |> Dict.toList
      |> List.map .0

  pipes : Dict Coord Pipe
  pipes =
    cells
      |> Dict.toList
      |> List.keepOks \(coord, cell) ->
          when cell is
            Pipe pipe -> Ok (coord, pipe)
            _ -> Err NotAPipe
      |> Dict.fromList

  when starts is
    [] -> Err NoStart
    [_, _, ..] -> Err MoreThanOneStart
    [start] ->
      pipeList = pipes |> Dict.toList
      hasNorth = pipeList |> List.any \(coord, pipe) ->
        if (coord |> isNorthOf start) && (pipe |> Pipe.hasSouthConnection) then
          dbg coord
          dbg pipe
          Bool.true
        else
          Bool.false
      hasEast = pipeList |> List.any \(coord, pipe) -> (coord |> isEastOf start) && (pipe |> Pipe.hasWestConnection)
      hasSouth = pipeList |> List.any \(coord, pipe) -> (coord |> isSouthOf start) && (pipe |> Pipe.hasNorthConnection)
      hasWest = pipeList |> List.any \(coord, pipe) -> (coord |> isWestOf start) && (pipe |> Pipe.hasEastConnection)
      startType =
        if hasNorth && hasSouth then
          NorthToSouth
        else if hasNorth then
          if hasEast then
            NorthToEast
          else
            #expect hasWest
            NorthToWest
        else if hasSouth then
          if hasEast then
            SouthToEast
          else
            #expect hasWest
            SouthToWest
        else
          #expect hasEast
          #expect hasWest
          EastToWest
      Ok
        { pipes: pipes |> Dict.insert start startType
        , start: start
        , width
        , height
        }

surroundingPipes : PipeGrid, Coord -> List Coord
surroundingPipes = \grid, coord ->
  offsets =
    when grid.pipes |> Dict.get coord is
      Ok SouthToWest -> [(1, 0), (0, -1)]
      Ok SouthToEast -> [(1, 0), (0, 1)]
      Ok NorthToWest -> [(-1, 0), (0, -1)]
      Ok NorthToEast -> [(-1, 0), (0, 1)]
      Ok EastToWest -> [(0, 1), (0, -1)]
      Ok NorthToSouth -> [(1, 0), (-1, 0)]
      Err _ -> []
  offsets
    |> List.keepOks \offset -> coord |> addOffset offset
    |> List.keepIf \offsetCoord ->
        grid.pipes |> Dict.contains offsetCoord

isNorthOf = \(r1, c1), (r2, c2) -> r1 + 1 == r2 && c1 == c2
isSouthOf = \(r1, c1), (r2, c2) -> r1 == r2 + 1 && c1 == c2
isWestOf = \(r1, c1), (r2, c2) -> c1 + 1 == c2 && r1 == r2
isEastOf = \(r1, c1), (r2, c2) -> c1 == c2 + 1 && r1 == r2

connectedPipes : PipeGrid, Coord -> List Coord
connectedPipes = \grid, coord ->
  surroundingPipes grid coord
    |> List.keepIf \offsetCoord ->
        when (grid.pipes |> Dict.get coord, grid.pipes |> Dict.get offsetCoord) is
          (Ok p1, Ok p2) ->
            [ [ coord |> isNorthOf offsetCoord
              , p1 |> Pipe.hasSouthConnection
              , p2 |> Pipe.hasNorthConnection
              ]
            , [ coord |> isSouthOf offsetCoord
              , p1 |> Pipe.hasNorthConnection
              , p2 |> Pipe.hasSouthConnection
              ]
            , [ coord |> isEastOf offsetCoord
              , p1 |> Pipe.hasWestConnection
              , p2 |> Pipe.hasEastConnection
              ]
            , [ coord |> isWestOf offsetCoord
              , p1 |> Pipe.hasEastConnection
              , p2 |> Pipe.hasWestConnection
              ]
            ]
              |> List.any \conds -> List.all conds \c -> c
          _ -> crash "these cells are included by construction"

addOffset : Coord, Offset -> Result Coord [OutOfBounds]
addOffset = \(cr, cc), (or, oc) ->
  nr = Num.toI32 cr + or
  nc = Num.toI32 cc + oc
  when (Num.toNatChecked nr, Num.toNatChecked nc) is
    (Ok r, Ok c) -> Ok (r, c)
    _ -> Err OutOfBounds

findNext : PipeGrid, Coord, Coord -> Result Coord [StartHasNoAdjacentPipes, PipeDoesntLoop, CoordIsntPipe, NotConnectedToPipe, CoordOutOfBounds]
findNext = \grid, previousCoord, coord ->
  pipes = connectedPipes grid coord
  next = \isDirOf1, isDirOf2 ->
    if previousCoord |> isDirOf1 coord then
      pipes
        |> List.findFirst \pipe -> pipe |> isDirOf2 coord
        |> Result.mapErr \_ -> PipeDoesntLoop
    else if previousCoord |> isDirOf2 coord then
      pipes
        |> List.findFirst \pipe -> pipe |> isDirOf1 coord
        |> Result.mapErr \_ -> PipeDoesntLoop
    else
      Err NotConnectedToPipe

  cell <- Result.try (grid.pipes |> Dict.get coord |> Result.mapErr \_ -> CoordOutOfBounds)

  nextCell =
    when cell is
      SouthToWest -> next isSouthOf isWestOf
      SouthToEast -> next isSouthOf isEastOf
      NorthToWest -> next isNorthOf isWestOf
      NorthToEast -> next isNorthOf isEastOf
      EastToWest -> next isEastOf isWestOf
      NorthToSouth -> next isNorthOf isSouthOf
  nextCell

findPath : PipeGrid -> Result (List Coord) [
    CoordIsntPipe,
    CoordOutOfBounds,
    NotConnectedToPipe,
    PipeDoesntLoop,
    StartHasNoAdjacentPipes,
]
findPath = \grid ->
  previousFromStart =
    grid.pipes
      |> Dict.get grid.start
      |> Result.map \start ->
          when start is
            NorthToSouth | SouthToWest | SouthToEast -> (1, 0)
            NorthToWest | NorthToEast -> (-1, 0)
            EastToWest -> (0, 1)
      |> Result.try \offset ->
          grid.start |> addOffset offset
      |> Result.withDefault (0, 0)

  iter = \coord, previousCoord, coords ->
    if coords |> List.contains coord then
      Ok coords
    else
      when findNext grid previousCoord coord is
        Err e -> Err e
        Ok next ->
          iter next coord (coords |> List.append coord)

  iter grid.start previousFromStart []

