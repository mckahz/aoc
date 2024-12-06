interface Pipe
  exposes [Pipe, hasNorthConnection, hasEastConnection, hasSouthConnection, hasWestConnection]
  imports []

Pipe :
  [ SouthToWest
  , SouthToEast
  , NorthToWest
  , NorthToEast
  , EastToWest
  , NorthToSouth
  ]

hasNorthConnection = \cell -> [NorthToWest, NorthToSouth, NorthToEast] |> List.contains cell
hasSouthConnection = \cell -> [SouthToWest, NorthToSouth, SouthToEast] |> List.contains cell
hasEastConnection = \cell -> [NorthToEast, SouthToEast, EastToWest] |> List.contains cell
hasWestConnection = \cell -> [NorthToWest, SouthToWest, EastToWest] |> List.contains cell
