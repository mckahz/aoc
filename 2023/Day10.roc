interface Day10.Solution
    exposes
      [ parse
      , input, test1, test2, test3, test4, test5, test6
      , part1, part2
      , part1Test1Ans, part1Test2Ans, part2Test1Ans, part2Test2Ans, part2Test3Ans, part2Test4Ans, part2Test5Ans, part2Test6Ans
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test1.txt" as test1 : Str
      , "test2.txt" as test2 : Str
      , "test3.txt" as test3 : Str
      , "test4.txt" as test4 : Str
      , "test5.txt" as test5 : Str
      , "test6.txt" as test6 : Str
      , PipeGrid.{ PipeGrid }
      , Cell
      ]

part1Test1Ans = 4
part1Test2Ans = 8
part2Test1Ans = 1
part2Test2Ans = 1
part2Test3Ans = 4
part2Test4Ans = 4
part2Test5Ans = 8
part2Test6Ans = 10
part1Ans = 6907
part2Ans = 541

parse : Str -> PipeGrid
parse = \str ->
  when PipeGrid.fromStr str is
    Ok grid -> grid
    Err _ -> crash "invalid input"

part1 : PipeGrid -> Nat
part1 = \grid ->
  path =
    grid
      |> PipeGrid.findPath
      |> Result.withDefault []
  List.len path // 2

part2 : PipeGrid -> Nat
part2 = \grid ->
  path =
    pipeCoords =
      grid
        |> PipeGrid.findPath
        |> Result.withDefault []

    pipes =
      pipeCoords
        |> List.keepOks \coord -> grid.pipes |> Dict.get coord

    List.map2 pipeCoords pipes \coord, pipe ->
        (coord, pipe)
      |> Dict.fromList

  coords =
    r <- List.joinMap (List.range { start: At 0, end: Length grid.height })
    c <- List.joinMap (List.range { start: At 0, end: Length grid.width })
    List.single (r, c)

  isInsideLoop = \(r, c) ->
    List.range { start: At 0, end: Before c }
      |> List.walkBackwards (None, 0) \(lastCorner, boundaries), tc ->
          path
            |> Dict.get (r, tc)
            |> Result.map \pipe ->
                when (lastCorner, pipe) is
                  (_, NorthToWest) -> (North, boundaries)
                  (_, SouthToWest) -> (South, boundaries)
                  (South, NorthToEast) | (North, SouthToEast) -> (None, boundaries + 1)
                  (_, NorthToEast) | (_, SouthToEast) -> (None, boundaries)
                  (_, EastToWest) -> (lastCorner, boundaries)
                  (_, NorthToSouth) -> (None, boundaries + 1)
            |> Result.withDefault (lastCorner, boundaries)
      |> .1
      |> Num.isOdd

  coords
    |> List.dropIf \coord -> path |> Dict.contains coord
    |> List.countIf isInsideLoop
