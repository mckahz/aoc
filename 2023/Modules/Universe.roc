interface Universe
  exposes
    [ Universe
    , fromStr
    , toStr
    , findEmptyRows
    , findEmptyCols
    , galaxyPairs
    ]
  imports [Matrix.{ Matrix, Coord }]

Universe : Matrix [Galaxy, Space]

fromStr : Str -> Result Universe [ContainsUnknownGrapheme]
fromStr = \str ->
  line <- List.mapTry (
    str
      |> Str.split "\n"
      |> List.dropLast 1
  )

  grapheme <- List.mapTry (Str.graphemes line)

  when grapheme is
    "#" -> Ok Galaxy
    "." -> Ok Space
    _ -> Err ContainsUnknownGrapheme

lineToStr = \line ->
  line
    |> List.map thingToStr
    |> Str.joinWith ""

thingToStr = \thing ->
  when thing is
    Galaxy -> "#"
    Space -> "."

toStr : Universe -> Str
toStr = \universe ->
  universe
    |> List.map lineToStr
    |> Str.joinWith "\n"

# this produces a compiler bug
#getHeight : Matrix elem -> Nat
#getHeight = List.len

findEmptyRows : Universe -> List Nat
findEmptyRows = \universe ->
  List.range { start: At 0, end: Length (Matrix.getHeight universe) }
    |> List.map2 universe \i, line -> (i, line)
    |> List.dropIf \(_, line) ->
        line |> List.any \thing -> thing == Galaxy
    |> List.map .0

findEmptyCols : Universe -> List Nat
findEmptyCols = \universe ->
  universe
    |> Matrix.transpose
    |> findEmptyRows

galaxyCoords : Universe -> List Coord
galaxyCoords = \universe ->
  width = Matrix.getWidth universe
  height = Matrix.getHeight universe

  things = List.join universe

  List.range { start: At 0, end: Length (width * height) }
    |> List.map2 things \i, thing ->
        ((i // width, i % width), thing)
    |> List.keepIf \(_, thing) -> thing == Galaxy
    |> List.map .0

galaxyPairs : Universe -> List (Coord, Coord)
galaxyPairs = \universe ->
  coords = galaxyCoords universe
  c1 <- List.joinMap coords
  c2 <- List.joinMap (
    idx =
      coords
        |> List.findFirstIndex \coord -> coord == c1
        |> Result.withDefault 0
    coords
      |> List.dropFirst (idx + 1)
  )
  {} <- List.joinMap (if c1 == c2 then [] else [{}])
  List.single (c1, c2)

