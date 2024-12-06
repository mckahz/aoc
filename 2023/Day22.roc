interface Day22.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      , part1TestAns, part2TestAns
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , Snapshot.{ Snapshot, Brick }
      ]

part1TestAns = 5
part1Ans = 437
part2TestAns = 7
part2Ans = 42561

parse : Str -> Snapshot
parse = \str ->
  when Snapshot.fromStr str is
    Ok s -> s
    Err e ->
      dbg e
      crash "invalid input"

willFallOn : Brick, Brick -> Bool
willFallOn = \b1, b2 ->
  contains : Brick, { x: Nat, y: Nat } -> Bool
  contains = \brick, { x, y } ->
    when brick.direction is
      None | Z -> brick.lo.x == x && brick.lo.y == y
      X -> brick.lo.x <= x && x <= brick.hi.x && brick.lo.y == y
      Y -> brick.lo.y <= y && y <= brick.hi.y && brick.lo.x == x

  point =
    when (b1.direction, b2.direction) is
      (X, X) -> { x: Num.max b1.lo.x b2.lo.x, y: b1.lo.y }
      (Y, Y) -> { x: b1.lo.x, y: Num.max b1.lo.y b2.lo.y }
      (X, Y) -> { x: b2.lo.x, y: b1.lo.y }
      (Y, X) -> { x: b1.lo.x, y: b2.lo.y }

      (None, _) | (Z, _) -> { x: b1.lo.x, y: b1.lo.y }
      (_, None) | (_, Z) -> { x: b2.lo.x, y: b2.lo.y }

  (b1.lo.z > b2.hi.z)
  && (b1 |> contains point)
  && (b2 |> contains point)

isDirectlyAbove : Brick, Brick -> Bool
isDirectlyAbove = \b1, b2 ->
  b1.lo.z == b2.hi.z + 1

isSupporting : Brick, Brick -> Bool
isSupporting = \b1, b2 ->
  (b2 |> isDirectlyAbove b1)
  && (b2 |> willFallOn b1)

collapse : Snapshot -> Snapshot
collapse = \snapshot ->
  move : Brick, Nat -> Brick
  move = \brick, z ->
    lo = brick.lo
    hi = brick.hi
    { brick
    & lo: { lo & z }
    , hi: { hi & z: z + hi.z - lo.z }
    }

  ascending =
    snapshot
    |> List.sortWith \b1, b2 ->
        Num.compare b1.lo.z b2.lo.z

  List.walk ascending [] \collapsed, brick ->
    z =
      collapsed
      |> List.keepIf \b -> brick |> willFallOn b
      |> List.map .hi
      |> List.map .z
      |> List.max
      |> Result.withDefault 0
      |> Num.add 1

    collapsed |> List.append (move brick z)

getSupporting : Snapshot, Brick -> List Brick
getSupporting = \snapshot, brick ->
  snapshot
  |> List.keepIf \otherBrick -> brick |> isSupporting otherBrick

part1 : Snapshot -> Nat
part1 = \snapshot ->
  collapsed = collapse snapshot

  supported : Dict Brick (List Brick)
  supported =
    collapsed
    |> List.map \brick -> (brick, getSupporting collapsed brick)
    |> Dict.fromList

  canDisintegrate : Brick -> Bool
  canDisintegrate = \brick ->
    supportedByBrick =
      Dict.get supported brick
      |> Result.withDefault []

    supportedByOther =
      supported
      |> Dict.remove brick
      |> Dict.values
      |> List.join

    List.all supportedByBrick \b -> supportedByOther |> List.contains b

  collapsed |> List.countIf canDisintegrate

part2 : Snapshot -> Nat
part2 = \snapshot ->
  collapsed = collapse snapshot

  chainReaction : List Brick, List Brick -> List Brick
  chainReaction = \oldFalling, currentFalling ->
    # maybe only check those which are at a certain hight, to account for vertical bricks fucking up the calculation?
    dropFalling : List Brick -> List Brick
    dropFalling = \bricks ->
      bricks
      |> List.dropIf \brick -> oldFalling |> List.contains brick
      |> List.dropIf \brick -> currentFalling |> List.contains brick

    maybeFalling =
      currentFalling
      |> List.joinMap \brick -> getSupporting collapsed brick
      |> dropFalling

    supported =
      collapsed
      |> dropFalling
      |> List.joinMap \brick -> getSupporting collapsed brick

    newFalling =
      maybeFalling
      |> List.dropIf \brick -> supported |> List.contains brick
      |> Set.fromList
      |> Set.toList

    if newFalling |> List.isEmpty then
      List.concat oldFalling currentFalling
    else
      chainReaction (List.concat oldFalling currentFalling) newFalling

  collapsed
  |> List.mapWithIndex \brick, i ->
      progress = "\(Num.toStr (i + 1))/\(Num.toStr (List.len snapshot))"
      dbg progress
      chainReaction [] (List.single brick)
  |> List.map \s -> List.len s - 1
  |> List.sum
