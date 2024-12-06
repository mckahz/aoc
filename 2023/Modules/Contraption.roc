interface Contraption
  exposes [Contraption, fromStr, toStr]
  imports []

Contraption :
  { width : I16
  , height : I16
  , directors : Dict Coord Director
  }
Coord : (I16, I16)
Director : [FSlash, BSlash, VSplitter, HSplitter]

fromStr : Str -> Result Contraption [ContainsInvalidString]
fromStr = \str ->
  lines =
    str
    |> Str.split "\n"
    |> List.dropLast 1
  width =
    lines
    |> List.first
    |> Result.withDefault ""
    |> Str.countGraphemes
    |> Num.toI16
  height =
    lines
    |> List.len
    |> Num.toI16
  # directors
  lines
  |> List.mapTry \line ->
      line
      |> Str.graphemes
      |> List.mapTry \grapheme ->
          when grapheme is
            "." -> Ok Empty
            "\\" -> Ok (Director BSlash)
            "/" -> Ok (Director FSlash)
            "|" -> Ok (Director VSplitter)
            "-" -> Ok (Director HSplitter)
            _ -> Err ContainsInvalidString
  |> Result.map \matrix ->
      directors =
        matrix
        |> List.mapWithIndex \line, i ->
            line
            |> List.mapWithIndex \director, j ->
                ((Num.toI16 i, Num.toI16 j), director)
        |> List.join
        |> List.keepOks \(pos, x) ->
            when x is
              Empty -> Err NotDirector
              Director director -> Ok (pos, director)
        |> Dict.fromList

      { directors, width, height }

toStr : Contraption -> Str
toStr = \{ directors, width, height } ->
  List.range { start: At 0, end: Length (Num.toNat height) }
  |> List.map \i ->
      List.range { start: At 0, end: Length (Num.toNat width) }
      |> List.map \j ->
          when directors |> Dict.get (i, j) is
            Ok BSlash -> "\\"
            Ok FSlash -> "/"
            Ok VSplitter -> "|"
            Ok HSplitter -> "-"
            Err _ -> "."
      |> Str.joinWith ""
  |> List.prepend ""
  |> Str.joinWith "\n"
