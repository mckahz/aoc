interface Matrix
  exposes [Matrix, taxiCab, transpose, getWidth, getHeight]
  imports []


Matrix elem : List (List elem)
Coord : (Nat, Nat)

taxiCab : Coord, Coord -> Nat
taxiCab = \(r1, c1), (r2, c2) ->
  Num.toNat (Num.abs (Num.toI32 r1 - Num.toI32 r2))
    + Num.toNat (Num.abs (Num.toI32 c1 - Num.toI32 c2))

transpose : Matrix elem -> Matrix elem
transpose = \matrix ->
  List.range { start: At 0, end: Length (getWidth matrix) }
    |> List.walk [] \m, i ->
        col =
          List.map matrix \row ->
            result =
              row
                |> List.dropFirst i
                |> List.first
            when result is
              Ok elem -> elem
              Err _ -> crash "since i is the smallest length we will always find this element"
        m |> List.append col

getWidth : Matrix elem -> Nat
getWidth = \universe ->
  universe
    |> List.map List.len
    |> List.min
    |> Result.withDefault 0

getHeight : Matrix elem -> Nat
getHeight = \universe ->
  universe
    |> List.len
