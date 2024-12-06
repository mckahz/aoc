interface Snapshot
  exposes [Snapshot, Brick, Snapshot, fromStr, toStr]
  imports []


Snapshot : List Brick
Position : { x: Nat, y: Nat, z: Nat }
Brick :
  { lo: Position
  , hi: Position
  , direction: Direction
  }
Direction: [X, Y, Z, None]

fromStr : Str -> Result Snapshot [InvalidBrick, NotEnoughCoords, TooManyCoords, TooManyPositions]
fromStr = \str ->
  position = \s ->
    when s |> Str.split "," |> List.keepOks Str.toNat is
      [x, y, z] ->
        Ok { x, y, z }
      [] | [_] | [_, _] -> Err NotEnoughCoords
      _ -> Err TooManyCoords

  brick : Str -> Result Brick [InvalidBrick, NotEnoughCoords, TooManyCoords, TooManyPositions]
  brick = \s ->
    when s |> Str.split "~" is
      [position1, position2] ->
        p1 <- Result.try (position position1)
        p2 <- Result.try (position position2)

        onX = p1.z == p2.z && p1.y == p2.y
        onY = p1.x == p2.x && p1.z == p2.z
        onZ = p1.x == p2.x && p1.y == p2.y

        if onX && onY && onZ then
          Ok { lo: p1, hi: p2, direction: None }
        else if onX then
          if p1.x < p2.x then
            Ok { lo: p1, hi: p2, direction: X }
          else
            Ok { lo: p2, hi: p1, direction: X }
        else if onY then
          if p1.y < p2.y then
            Ok { lo: p1, hi: p2, direction: Y }
          else
            Ok { lo: p2, hi: p1, direction: Y }
        else if onZ then
          if p1.z < p2.z then
            Ok { lo: p1, hi: p2, direction: Z }
          else
            Ok { lo: p2, hi: p1, direction: Z }
        else
          Err InvalidBrick
      _ -> Err TooManyPositions

  str
  |> Str.split "\n"
  |> List.dropLast 1
  |> List.mapTry brick

toStr : Snapshot -> Str
toStr = \snapshot ->
  highest = \getter ->
    snapshot
    |> List.map .hi
    |> List.map getter
    |> List.max
    |> Result.withDefault 0

  hX = highest .x
  hY = highest .y
  hZ = highest .z

  List.range { start: At 1, end: At hZ }
  |> List.reverse
  |> List.map \r ->
      List.range { start: At 0, end: At hX }
      |> List.map \c ->
          appears =
            snapshot
            |> List.findFirst \brick ->
                brick.lo.z <= r && r <= brick.hi.z
                && brick.lo.x <= c && c <= brick.hi.x
            |> Result.isOk
          if appears then
            "O"
          else
            "."
      |> Str.joinWith ""
  |> List.prepend ""
  |> Str.joinWith "\n"
