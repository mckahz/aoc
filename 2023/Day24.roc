# TODO: Actually implement Gauss-Jordan Elimination
interface Day24.Solution
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
      , Vec3.{ Vec3 }
      ]

part1TestAns = 2 # (7, 27)
part1Ans = 15262 # (200000000000000, 400000000000000)
part2TestAns = 47
part2Ans = 695832176624149

Hailstone :
  { position: Vec3
  , velocity: Vec3
  }

parse : Str -> List Hailstone
parse = \str ->
  hailstones =
    str
    |> Str.split "\n"
    |> List.dropLast 1
    |> List.mapTry \line ->
        when line |> Str.split "@" is
          [p, v] ->
            position <- Result.try (Vec3.fromStr p)
            velocity <- Result.try (Vec3.fromStr v)
            Ok { position, velocity }
          _ ->
            Err NotAHailstone

  when hailstones is
    Ok h -> h
    Err e ->
      dbg e
      crash "invalid input"

Bounds : (F64, F64)

collides : Hailstone, Hailstone, Bounds -> Bool
collides = \h1, h2, (low, high) ->
  dx = h2.position.x - h1.position.x
  dy = h2.position.y - h1.position.y

  det = h2.velocity.x * h1.velocity.y - h2.velocity.y * h1.velocity.x
  u = (dy * h2.velocity.x - dx * h2.velocity.y) / det
  v = (dy * h1.velocity.x - dx * h1.velocity.y) / det

  p1 = h1.position |> Vec3.add (h1.velocity |> Vec3.scale u)
  p2 = h2.position |> Vec3.add (h2.velocity |> Vec3.scale v)

  dp = Vec3.abs (p1 |> Vec3.sub p2)

  x = p1.x
  y = p1.y

  dp.x < 1 && dp.y < 1
  && u > 0 && v > 0
  && low <= x && x <= high
  && low <= y && y <= high


part1 : Bounds, List Hailstone -> Nat
part1 = \bounds, hailstones ->
  indices = List.range { start: At 0, end: Length (List.len hailstones) }

  collisions =
    i <- List.joinMap indices
    j <- List.keepOks (indices |> List.dropFirst (i + 1))

    (hailstone1, hailstone2) =
      when (hailstones |> List.get i, hailstones |> List.get j) is
        (Ok h1, Ok h2) -> (h1, h2)
        _ -> crash ""

    if hailstone1 |> collides hailstone2 bounds then
      Ok {}
    else
      Err NoCollision

  List.len collisions

# (dy'-dy) X + (dx-dx') Y + (y-y') DX + (x'-x) DY = x' dy' - y' dx' - x dy + y dx

#Matrix a : List (List a)
#
#triangulate : Matrix a -> Matrix a
#triangulate = \matrix ->
#  iter : { matrix: Matrix a, rowIdx: Nat, lowerRowIdx: Nat, mostRecentlySwapped: [Not, Swapped Nat] } -> Matrix a
#  iter = \state ->
#    if state.rowIdx >= List.len matrix then
#      state.matrix
#    else if lowerRowIdx >= List.len matrix then
#      rowIdx = state.rowIdx + 1
#      iter
#        { state
#        & rowIdx
#        , lowerRowIdx: rowIdx + 1
#        , mostRecentlySwapped: Not
#        }
#    else
#      row = state.matrix |> List.get rowIdx |> Result.withDefault []
#      lowerRow = state.matrix |> List.get lowerRowIdx |> Result.withDefault []
#
#      when lowerRow |> List.get rowIdx is
#        Err _ -> crash ""
#        Ok 0 ->
#          # todo: terminate
#          swap =
#            when state.mostRecentlySwapped is
#              Swapped s -> s + 1
#              Not -> rowIdx + 1
#          iter
#            { state
#            & matrix: state.matrix |> swapRows swap lowerRowIdx
#            , mostRecentlySwapped: Swapped swap
#            }
#        Ok x ->
#          startRow = row |> List.get rowIdx
#          startLowerRow = lowerRow |> List.get rowIdx
#          c = -startLowerRow / startRow
#          rowIdx = state.rowIdx + 1
#          iter
#            { state
#            & matrix: state.matrix |> addRow c rowIdx lowerRowIdx
#            , rowIdx
#            , lowerRowIdx: rowIdx + 1
#            }
#
#  iter { rowIdx: 0, matrix }
#
#gaussJordan : Matrix a -> List a
#gaussJordan = \matrix ->
#  matrix
#  |> triangulate
#  |> triangulateBackwards
#
#swapRows : Matrix a, Nat, Nat-> Matrix a
#swapRows = \matrix, r1, r2 ->
#  when (matrix |> List.get r1, matrix |> List.get r2) is
#    (Ok row1, Ok row2) ->
#      matrix
#      |> List.set r1 row2
#      |> List.set r2 row1
#    _ -> crash ""
#
#scaleRows : Matrix a, Nat, a -> Matrix a
#scaleRows = \matrix, r, c ->
#  List.update matrix r \row ->
#    List.map row \cell ->
#      cell * c
#
#addRow : Matrix a, a, a, a -> Matrix a
#addRow = \matrix, c, r1, r2 ->
#  when matrix |> List.get r1 is
#    Ok row ->
#      List.update matrix r2 \oldRow ->
#        List.map2 oldRow row \oldCell, cell ->
#          oldCell + c * cell

# (dy'-dy) X + (dx-dx') Y + (y-y') DX + (x'-x) DY = x' dy' - y' dx' - x dy + y dx
# coefficients = \x1, y1, dx1, dy1, x2, y2, dx2, dy2 ->

coefficients : F64, F64, F64, F64, F64, F64, F64, F64 -> List F64
coefficients = \x1, y1, dx1, dy1, x2, y2, dx2, dy2 ->
  constant = x1 * dy1 - y1 * dx1 + y2 * dx2 - x2 * dy2
  [dy1 - dy2, dx2 - dx1, y2 - y1, x1 - x2, constant]

part2 : List Hailstone -> Nat
part2 = \hailstones ->
  firstFew = hailstones |> List.takeFirst 5

  xyc =
    List.map2 firstFew (firstFew |> List.dropFirst 1) \h1, h2 ->
      coefficients
        h1.position.x
        h1.position.y
        h1.velocity.x
        h1.velocity.y
        h2.position.x
        h2.position.y
        h2.velocity.x
        h2.velocity.y

  yzc = List.map2 firstFew (firstFew |> List.dropFirst 1) \h1, h2 ->
      coefficients
        h1.position.y
        h1.position.z
        h1.velocity.y
        h1.velocity.z
        h2.position.y
        h2.position.z
        h2.velocity.y
        h2.velocity.z

  _ =
    xyc
    |> List.map \xyr ->
        dbg xyr
        {}

  _ =
    yzc
    |> List.map \yzr ->
        dbg yzr
        {}

  0
