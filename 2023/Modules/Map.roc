interface Map
  exposes [Direction, Position, Map, fromStr, show, safelyStep]
  imports
    [ Math.{ mod }
    ]

Direction : [Left, Right, Up, Down]
Position : (I64, I64)
Map :
  { start: Position
  , rocks: Set Position
  , size: Nat
  }

fromStr : Str -> Result Map [NoStart, TooManyStarts, EmptyMap, NonSquareMap]
fromStr = \str ->
  things =
    str
    |> Str.split "\n"
    |> List.dropLast 1
    |> List.mapWithIndex \row, i ->
        row
        |> Str.graphemes
        |> List.mapWithIndex \grapheme, j ->
            pos : Position
            pos = (Num.toI64 i, Num.toI64 j)
            when grapheme is
              "S" -> Start pos
              _ if grapheme == hash -> Rock pos
              _ -> Other
    |> List.join
    |> Set.fromList

  start <- Result.try (
    starts =
      thing <- Set.joinMap things
      when thing is
        Start pos -> Set.single pos
        _ -> Set.empty {}

    when starts |> Set.toList is
      [] -> Err NoStart
      [_, _, ..] -> Err TooManyStarts
      [only] -> Ok only
  )

  rocks =
    thing <- Set.joinMap things
    when thing is
      Rock pos -> Set.single pos
      _ -> Set.empty {}

  width <- Result.try (
    str
    |> Str.split "\n"
    |> List.first
    |> Result.mapErr \_ -> EmptyMap
    |> Result.map Str.countGraphemes
  )

  height <- Result.try (
    h =
      str
      |> Str.split "\n"
      |> List.dropLast 1
      |> List.len
    if h == 0 then
      Err EmptyMap
    else
      Ok (Num.toNat h)
  )

  {} <- Result.try (if width != height then Err NonSquareMap else Ok {})

  size = width # or height

  Ok { start, rocks, size }

safelyStep : Position, Map, Direction -> Result Position [Obstructed]
safelyStep = \(row, col), map, direction ->
  newPos =
    when direction is
      Up -> (row - 1, col)
      Left -> (row, col - 1)
      Down -> (row + 1, col)
      Right -> (row, col + 1)

  {} <- Result.try (
    if map.rocks |> Set.contains (newPos |> wrapped map) then
      Err Obstructed
    else
      Ok {}
  )

  Ok newPos

wrapped : Position, Map -> Position
wrapped = \(r, c), map ->
  size = Num.toI64 map.size
  (r |> mod size, c |> mod size)

show : Map, Set Position -> Str
show = \map, positions ->
  size = Num.toI64 map.size

  graphemes =
    row <- List.joinMap (List.range { start: At 0, end: Length map.size })
    col <- List.map (List.range { start: At 0, end: Length map.size })

    pos = (Num.toI64 row, Num.toI64 col)

    isPosition =
      positions
      |> Set.map \(r, c) -> (r |> mod size, c |> mod size)
      |> Set.contains pos

    isRock =
      map.rocks
      |> Set.contains pos

    if isPosition && isRock then
      "?"
    else if isRock then
      hash
    else if isPosition then
      "O"
    else
      "."

  graphemes
  |> List.chunksOf map.size
  |> List.map \row -> row |> Str.joinWith ""
  |> List.prepend ""
  |> Str.joinWith "\n"

hash = "#"
