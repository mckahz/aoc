interface Day03.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , Day03.Schematic.{ Schematic, getChunks }
      ]


parse : Str -> Schematic
parse = \str ->
  str
    |> Str.split "\n"
    |> List.map Str.graphemes
    |> List.dropLast 1


part1 : Str -> Nat
part1 = \s ->
  schematic = parse s
  partNumbers =
    chunk <- List.joinMap (getChunks schematic)
    when chunk is
      Part _ str ->
        str
          |> Str.toNat
          |> Result.map List.single
          |> Result.withDefault []
      _ -> []
  List.sum partNumbers

part2 : Str -> Nat
part2 = \s ->
  schematic = parse s
  parts =
    List.joinMap (getChunks schematic) \chunk ->
      when chunk is
        Part coords str -> [(coords, str |> Str.toNat |> Result.withDefault 0)]
        _ -> []

  gears =
    List.joinMap (getChunks schematic) \chunk ->
      when chunk is
        Gear coord -> [coord]
        _ -> []

  List.joinMap gears \(gr, gc) ->
      adjacentParts =
        parts
          |> List.keepIf \(coords, _) ->
              List.any coords \(nr, nc) -> gr - 1 <= nr && nr <= gr + 1 && gc - 1 <= nc && nc <= gc + 1
          |> List.map .1
      when adjacentParts is
        [x, y] -> [x * y]
        _ -> []
  |> List.sum
