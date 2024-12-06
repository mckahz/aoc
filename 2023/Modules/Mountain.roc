interface Mountain
  exposes [Mountain, fromStr, toStr, showPath, move, movePowerfully]
  imports []

Mountain :
  { rocks: Set Position
  , slopes: Dict Position Direction
  , width: Nat
  , height: Nat
  , start: Position
  , end: Position
  }
Position : { row: Nat, col: Nat }
Direction : [Left, Right, Up, Down]

fromStr : Str -> Result Mountain [InvalidMountain]
fromStr = \str ->
  rows =
    str
    |> Str.split "\n"
    |> List.dropLast 1

  land <- Result.try (
    rows
    |> List.mapWithIndex \line, row ->
        line
        |> Str.graphemes
        |> List.mapWithIndex \grapheme, col ->
            when grapheme is
              "#" -> Ok [({ row, col }, Rock)]
              ">" -> Ok [({ row, col }, Slope Right)]
              "<" -> Ok [({ row, col }, Slope Left)]
              "^" -> Ok [({ row, col }, Slope Up)]
              "v" -> Ok [({ row, col }, Slope Down)]
              "." -> Ok []
              _ -> Err InvalidMountain
        |> List.mapTry \x -> x
        |> Result.map List.join
    |> List.mapTry \x -> x
    |> Result.map List.join
  )

  slopes =
    land
    |> List.keepOks \(pos, type) ->
        when type is
          Slope x -> Ok (pos, x)
          _ -> Err {}
    |> Dict.fromList

  rocks =
    land
    |> List.keepOks \(pos, type) ->
        when type is
          Rock -> Ok pos
          _ -> Err {}
    |> Set.fromList

  width =
    rows
    |> List.first
    |> Result.map Str.countGraphemes
    |> Result.withDefault 0

  height =
    rows
    |> List.len

  Ok { rocks, slopes, width, height, start: { row: 0, col: 1 }, end: { row: height - 1, col: width - 2 } }

showPath : Mountain, List Position -> Str
showPath = \mountain, positions ->
  graphemes =
    row <- List.joinMap (List.range { start: At 0, end: Length mountain.height })
    col <- List.map (List.range { start: At 0, end: Length mountain.width })
    pos = { row, col }
    matchingPositions =
      positions |> List.countIf \position -> position == pos

    if mountain.rocks |> Set.contains pos then
      hash
    else if matchingPositions == 1 then
      "O"
    else if matchingPositions > 1 then
      "?"
    else if pos == mountain.start then
      "S"
    else
      when mountain.slopes |> Dict.get pos is
        Ok Right -> ">"
        Ok Left -> "<"
        Ok Up -> "^"
        Ok Down -> "v"
        Err _ -> "."

  graphemes
  |> List.chunksOf mountain.width
  |> List.map \line -> line |> Str.joinWith ""
  |> List.prepend ""
  |> Str.joinWith "\n"

toStr : Mountain -> Str
toStr = \mountain ->
  graphemes =
    row <- List.joinMap (List.range { start: At 0, end: Length mountain.height })
    col <- List.map (List.range { start: At 0, end: Length mountain.width })
    pos = { row, col }
    if mountain.rocks |> Set.contains pos then
      hash
    else
      when mountain.slopes |> Dict.get pos is
        Ok Right -> ">"
        Ok Left -> "<"
        Ok Up -> "^"
        Ok Down -> "v"
        Err _ -> "."

  graphemes
  |> List.chunksOf mountain.width
  |> List.map \line -> line |> Str.joinWith ""
  |> List.prepend ""
  |> Str.joinWith "\n"

movePowerfully : Mountain, Position, Direction -> Result Position [OutOfBounds, Obstructed]
movePowerfully = \mountain, pos, direction ->
  {} <- Result.try (
    when direction is
      Up if pos.row == 0 -> Err OutOfBounds
      Down if pos.row == mountain.height - 1 -> Err OutOfBounds
      Left if pos.col == 0 -> Err OutOfBounds
      Right if pos.col == mountain.width - 1 -> Err OutOfBounds
      _ -> Ok {}
  )

  newPos =
    when direction is
      Up -> { pos & row : pos.row - 1 }
      Down -> { pos & row : pos.row + 1 }
      Left -> { pos & col : pos.col - 1 }
      Right -> { pos & col : pos.col + 1 }

  {} <- Result.try (
    if mountain.rocks |> Set.contains newPos then
      Err Obstructed
    else
      Ok {}
  )

  Ok newPos

move : Mountain, Position, Direction -> Result Position [OutOfBounds, Obstructed, UpSlope, AwayFromSlope]
move = \mountain, pos, direction ->
  {} <- Result.try (
    when mountain.slopes |> Dict.get pos is
      Ok Up if direction != Up -> Err AwayFromSlope
      Ok Down if direction != Down -> Err AwayFromSlope
      Ok Left if direction != Left -> Err AwayFromSlope
      Ok Right if direction != Right -> Err AwayFromSlope
      _ -> Ok {}
  )

  {} <- Result.try (
    when direction is
      Up if pos.row == 0 -> Err OutOfBounds
      Down if pos.row == mountain.height - 1 -> Err OutOfBounds
      Left if pos.col == 0 -> Err OutOfBounds
      Right if pos.col == mountain.width - 1 -> Err OutOfBounds
      _ -> Ok {}
  )

  newPos =
    when direction is
      Up -> { pos & row : pos.row - 1 }
      Down -> { pos & row : pos.row + 1 }
      Left -> { pos & col : pos.col - 1 }
      Right -> { pos & col : pos.col + 1 }

  {} <- Result.try (
    if mountain.rocks |> Set.contains newPos then
      Err Obstructed
    else
      Ok {}
  )

  {} <- Result.try (
    when (mountain.slopes |> Dict.get newPos, direction) is
      (Ok Up, Down) -> Err UpSlope
      (Ok Down, Up) -> Err UpSlope
      (Ok Left, Right) -> Err UpSlope
      (Ok Right, Left) -> Err UpSlope
      _ -> Ok {}
  )

  Ok newPos

hash = "#"
