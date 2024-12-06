app "day23" # Day23.Solution
  packages {
    pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
  }
# exposes
#   [ input, test
#   , part1, part2
#   ]
  imports
    [ "input.txt" as input : Str
    , "test.txt" as test : Str
    , Mountain.{ Mountain, Position, Direction }
    , pf.Task
    ]
  provides [main] to pf

main = Task.ok {}

part1TestAns = 94
part1Ans = 2442
part2TestAns = 154
part2Ans = 6898

parse : Str -> Mountain
parse = \str ->
  when Mountain.fromStr str is
    Ok m -> m
    Err e ->
      dbg e
      crash "invalid input"

Directionality : [Directional, Bidirectional]

#ongestPath2 : Directionality, Mountain -> Nat
#ongestPath2 = \directionality, mountain ->
# dbg Starting "..."
#
# junctions : List Position
# junctions =
#   List.range { start: At 0, end: Length (mountain.width * mountain.height) }
#   |> List.keepOks \i ->
#       pos = { row: i // mountain.width, col: i % mountain.height }
#       {} <- Result.try (if mountain.rocks |> Set.contains pos then Err {} else Ok {})
#       adjacentLand =
#         [Left, Right, Up, Down]
#         |> List.countIf \direction ->
#             Mountain.movePowerfully mountain pos direction |> Result.isOk
#       if adjacentLand > 2 then
#         Ok pos
#       else
#         Err {}
#   |> List.prepend mountain.start
#   |> List.append mountain.end
#
# dbg Junctions
#
# junctionPaths : Dict (Position, Position) Nat
# junctionPaths =
#   paths =
#     start <- List.joinMap junctions
#     end <- List.joinMap junctions
#     {} <- List.keepOks (if start == end then [] else [{}])
#
#     next = \pos ->
#       [ Left, Right, Up, Down ]
#       |> List.keepOks \direction ->
#           Mountain.move mountain pos direction
#       |> List.dropIf \newPos ->
#           (junctions |> List.contains newPos)
#           && newPos != end
#
#     Path.dijkstra start end next \_, _ -> 1
#     |> Result.map List.len
#     |> Result.map \pathLength -> ((start, end), pathLength)
#   Dict.fromList paths
#
# dbg JunctionPaths
# dbg junctionPaths
#
# nextJunctions =
#   when directionality is
#     Directional -> \junction ->
#       junctionPaths
#       |> Dict.keys
#       |> List.keepOks \(from, to) ->
#           if from == junction then
#             Ok to
#           else
#             Err {}
#
#     Bidirectional -> \junction ->
#       junctionPaths
#       |> Dict.keys
#       |> List.keepOks \(from, to) ->
#           if from == junction then
#             Ok to
#           else if to == junction then
#             Ok from
#           else
#             Err {}
#
# weight : Position, Position -> Nat
# weight = \start, end ->
#   forward =
#     junctionPaths
#     |> Dict.get (start, end)
#   backward =
#     junctionPaths
#     |> Dict.get (end, start)
#   when (forward, backward) is
#     (Ok path, _) -> path - 1
#     (_, Ok path) -> path - 1
#     _ -> crash "there should be a path"
#
# longest =
#   path <- Result.try (Path.longest mountain.start mountain.end nextJunctions weight)
#   List.map2 path (path |> List.dropFirst 1) weight
#   |> List.sum
#   |> Ok
#
# when longest is
#   Ok l -> l
#   Err e ->
#     dbg e
#     crash "no path found"

longestPath : Directionality, Mountain -> Nat
longestPath = \directionality, mountain ->
  longestPathHelp
    [ { len: 0, position: mountain.start, junctionsVisited: [], direction: Down } ]
    directionality
    mountain

Path : { len: Nat, position: Position, junctionsVisited: List Position, direction: Direction }

opposite : Direction, Direction -> Bool
opposite = \d1, d2 ->
  when (d1, d2) is
    (Up, Down) | (Down, Up) | (Left, Right) | (Right, Left) -> Bool.true
    _ -> Bool.false

longestPathHelp : List Path, Directionality, Mountain -> Nat
longestPathHelp = \exploring, directionality, mountain ->
  when exploring |> List.findFirst \path -> path.position == mountain.end is
    Ok path -> path.len
    Err _ ->
      newExploring : List Path
      newExploring =
        path <- List.joinMap exploring

        move =
          when directionality is
            Directional -> Mountain.move
            Bidirectional -> Mountain.movePowerfully

        steps : List (Direction, Position)
        steps =
          [ Left, Right, Up, Down ]
          |> List.dropIf \d -> d |> opposite path.direction
          |> List.keepOks \d ->
              move mountain path.position d
              |> Result.map \p -> (d, p)
          |> List.dropIf \(_, p) -> path.visitedJunctions |> List.contains p

        (direction, position) <- List.joinMap steps

        len : Nat
        len = path.len + 1

        if List.len steps == 0 then
          []
        else if List.len steps == 1 then
          [ { path & len, position, direction } ]
        else
          [ { path & len, position, direction
            , junctionsVisited: path.junctionsVisited |> List.append path.position
            }
          ]

      longestPathHelp newExploring directionality mountain




part1 : Mountain -> Nat
part1 = \mountain ->
  longestPath Directional mountain

part2 : Mountain -> Nat
part2 = \mountain ->
  longestPath Bidirectional mountain
