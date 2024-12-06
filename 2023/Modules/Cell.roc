interface Cell
  exposes [Cell, fromStr, toStr]
  imports [Pipe.{ Pipe }]

Cell :
  [ Pipe Pipe
  , Start
  , Ground
  ]

fromStr : Str -> Result Cell [NotCell]
fromStr = \str ->
  when str is
    "|" -> Ok (Pipe NorthToSouth)
    "-" -> Ok (Pipe EastToWest)
    "L" -> Ok (Pipe NorthToEast)
    "J" -> Ok (Pipe NorthToWest)
    "F" -> Ok (Pipe SouthToEast)
    "7" -> Ok (Pipe SouthToWest)
    "S" -> Ok Start
    "." -> Ok Ground
    _ -> Err NotCell

toStr : Cell -> Str
toStr = \cell ->
  when cell is
    Start -> "S"
    Ground -> "."
    Pipe NorthToSouth -> "|"
    Pipe EastToWest -> "-"
    Pipe NorthToEast -> "L"
    Pipe NorthToWest -> "J"
    Pipe SouthToEast -> "F"
    Pipe SouthToWest -> "7"

