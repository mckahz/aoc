interface Land
  exposes [Land, fromStr, toStr]
  imports []

Land : [Rock, Ash]

fromStr : Str -> Result Land [InvalidLand]
fromStr = \str ->
  when str is
    "#" -> Ok Rock
    "." -> Ok Ash
    _ -> Err InvalidLand

toStr : Land -> Str
toStr = \land ->
  when land is
    Rock -> "#"
    Ash -> "."


