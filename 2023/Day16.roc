interface Day16.Solution
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
      , Contraption.{ Contraption, Coord }
      ]

part1TestAns = 46
part1Ans = 7482
part2TestAns = 51
part2Ans = 7896

Direction : [North, East, South, West]
Ray :
  { position: Coord
  , direction: Direction
  }

parse : Str -> Contraption
parse = \str ->
  when str |> Contraption.fromStr is
    Ok contraption -> contraption
    Err e ->
      dbg e
      crash "Invalid input"

moveRay : Ray, Set Ray, Contraption -> Set Ray
moveRay = \ray, history, contraption ->
  position =
    when ray.direction is
      North -> (ray.position.0 - 1, ray.position.1)
      East -> (ray.position.0, ray.position.1 + 1)
      South -> (ray.position.0 + 1, ray.position.1)
      West -> (ray.position.0, ray.position.1 - 1)

  directions =
    when contraption.directors |> Dict.get position is
      Err _ ->
        Set.single ray.direction
      Ok BSlash ->
        direction =
          when ray.direction is
            North -> West
            East -> South
            South -> East
            West -> North
        Set.single direction
      Ok FSlash ->
        direction =
          when ray.direction is
            North -> East
            East -> North
            South -> West
            West -> South
        Set.single direction
      Ok VSplitter ->
        when ray.direction is
          North -> Set.single North
          South -> Set.single South
          East -> Set.fromList [North, South]
          West -> Set.fromList [North, South]
      Ok HSplitter ->
        when ray.direction is
          East -> Set.single East
          West -> Set.single West
          North -> Set.fromList [East, West]
          South -> Set.fromList [East, West]

  outOfBounds =
    Bool.false
      || position.0 < 0
      || position.0 >= contraption.height
      || position.1 < 0
      || position.1 >= contraption.width

  if outOfBounds then
    Set.empty {}
  else
    directions
    |> Set.map \direction ->
        { position, direction }
    |> Set.difference history

findVisitedCoords : Ray, Contraption -> Set Coord
findVisitedCoords = \firstRay, contraption ->
  iter : Set Ray, Set Ray -> Set Coord
  iter = \rays, history ->
    if rays |> Set.isEmpty then
      history |> Set.map .position
    else
      newRays = Set.joinMap rays \ray -> moveRay ray history contraption
      iter newRays (newRays |> Set.union history)

  iter (Set.single firstRay) (Set.single firstRay)
  |> Set.dropIf \position -> position == firstRay.position

part1 : Contraption -> Nat
part1 = \contraption ->
  firstRay = { position : (0, -1), direction: East }
  Set.len (findVisitedCoords firstRay contraption)

part2 : Contraption -> Nat
part2 = \contraption ->
  leftAndRight =
    List.range { start: At 0, end: Length (Num.toNat contraption.width) }
    |> List.joinMap \j -> [{ position: (-1, j), direction: South }, { position: (contraption.height, j), direction: North }]

  topAndBottom =
    List.range { start: At 0, end: Length (Num.toNat contraption.height) }
    |> List.joinMap \i -> [{ position: (i, -1), direction: East }, { position: (i, contraption.width), direction: West }]

  List.concat leftAndRight topAndBottom
  |> List.map \firstRay ->
      Set.len (findVisitedCoords firstRay contraption)
  |> List.max
  |> Result.withDefault 0
