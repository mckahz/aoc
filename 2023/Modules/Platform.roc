interface Platform
  exposes [Platform, Direction, fromStr, toStr, load, moveRocks]
  imports []

Platform:
  { roundRocks : List Coord
  , squareRocks : List Coord
  , width : Nat
  , height : Nat
  }
Direction : [North, South, East, West]
Coord : (I16, I16)

toStr : Platform -> Str
toStr = \platform ->
  List.range { start: At 0, end: Length platform.height }
    |> List.map \i ->
        List.range { start: At 0, end: Length platform.width }
          |> List.map \j ->
              if platform.roundRocks |> List.contains (i, j) then
                "O"
              else if platform.squareRocks |> List.contains (i, j) then
                "#"
              else
                "."
          |> Str.joinWith ""
    |> List.prepend ""
    |> Str.joinWith "\n"

fromStr : Str -> Platform
fromStr = \str ->
  lines =
    str
      |> Str.split "\n"
      |> List.dropLast 1

  allRocks =
    lines
      |> List.mapWithIndex \line, i ->
          line
            |> Str.graphemes
            |> List.mapWithIndex \cell, j ->
                when cell is
                  "#" -> Ok ((Num.toI16 i, Num.toI16 j), Square)
                  "O" -> Ok ((Num.toI16 i, Num.toI16 j), Round)
                  _ -> Err NotRock
            |> List.keepOks \x -> x
      |> List.join

  roundRocks =
    allRocks
      |> List.keepIf \(_, rock) -> rock == Round
      |> List.map .0

  squareRocks =
    allRocks
      |> List.keepIf \(_, rock) -> rock == Square
      |> List.map .0

  width = lines |> List.first |> Result.withDefault "" |> Str.countGraphemes
  height = lines |> List.len

  { roundRocks, squareRocks, width, height }

load : Platform -> Nat
load = \platform ->
  platform.roundRocks
    |> List.map .0
    |> List.map Num.toI64
    |> List.map \row -> Num.toI64 platform.height - row
    |> List.sum
    |> Num.toNat

moveRock : Platform, Direction, Coord -> Coord
moveRock = \platform, direction, roundPos ->
  when direction is
    North ->
      closestSquare =
        platform.squareRocks
          |> List.keepIf \squarePos ->
              (squarePos.1 == roundPos.1)
              && (squarePos.0 < roundPos.0)
          |> List.walk (-1, roundPos.1) \square1, square2 ->
              if square1.0 > square2.0 then square1 else square2

      roundRocksBelow =
        platform.roundRocks
          |> List.keepIf \otherRoundPos ->
              (otherRoundPos.1 == roundPos.1)
              && (otherRoundPos.0 < roundPos.0)
              && (otherRoundPos.0 > closestSquare.0)
          |> List.len

      (r, c) = closestSquare
      (r + (1 + Num.toI16 roundRocksBelow), c)

    South ->
      closestSquare =
        platform.squareRocks
          |> List.keepIf \squarePos ->
              (squarePos.1 == roundPos.1)
              && (squarePos.0 > roundPos.0)
          |> List.walk (Num.toI16 platform.height, roundPos.1) \square1, square2 ->
              if square1.0 < square2.0 then square1 else square2

      roundRocksBelow =
        platform.roundRocks
          |> List.keepIf \otherRoundPos ->
              (otherRoundPos.1 == roundPos.1)
              && (otherRoundPos.0 > roundPos.0)
              && (otherRoundPos.0 < closestSquare.0)
          |> List.len

      (r, c) = closestSquare
      (r - (1 + Num.toI16 roundRocksBelow), c)

    West ->
      closestSquare =
        platform.squareRocks
          |> List.keepIf \squarePos ->
              (squarePos.0 == roundPos.0)
              && (squarePos.1 < roundPos.1)
          |> List.walk (roundPos.0, -1) \square1, square2 ->
              if square1.1 > square2.1 then square1 else square2

      roundRocksBelow =
        platform.roundRocks
          |> List.keepIf \otherRoundPos ->
              (otherRoundPos.0 == roundPos.0)
              && (otherRoundPos.1 < roundPos.1)
              && (otherRoundPos.1 > closestSquare.1)
          |> List.len

      (r, c) = closestSquare
      (r, c + (1 + Num.toI16 roundRocksBelow))

    East ->
      closestSquare =
        platform.squareRocks
          |> List.keepIf \squarePos ->
              (squarePos.0 == roundPos.0)
              && (squarePos.1 > roundPos.1)
          |> List.walk (roundPos.0, Num.toI16 platform.width) \square1, square2 ->
              if square1.1 < square2.1 then square1 else square2

      roundRocksBelow =
        platform.roundRocks
          |> List.keepIf \otherRoundPos ->
              (otherRoundPos.0 == roundPos.0)
              && (otherRoundPos.1 > roundPos.1)
              && (otherRoundPos.1 < closestSquare.1)
          |> List.len

      (r, c) = closestSquare
      (r, c - (1 + Num.toI16 roundRocksBelow))

moveRocks : Platform, Direction -> Platform
moveRocks = \platform, direction ->
  { platform & roundRocks: platform.roundRocks |> List.map \roundPos -> moveRock platform direction roundPos }

