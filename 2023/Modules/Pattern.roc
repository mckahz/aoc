interface Pattern
  exposes [Pattern, fromStr, toStr]
  imports
    [ Land.{ Land }
    , Matrix.{ Matrix }
    ]

Pattern : Matrix Land

fromStr : Str -> Result Pattern [InvalidLand, ContainsEmptyRow]
fromStr = \str ->
  str
    |> Str.split "\n"
    |> List.mapTry \row ->
        if Str.trim row == "" then
          Err ContainsEmptyRow
        else
          row
            |> Str.graphemes
            |> List.mapTry Land.fromStr

toStr : Pattern -> Str
toStr = \pattern ->
  pattern
    |> List.map \line ->
        line
          |> List.map Land.toStr
          |> Str.joinWith ""
    |> Str.joinWith "\n"
