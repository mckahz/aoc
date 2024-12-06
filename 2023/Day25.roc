interface Day25.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      , part1TestAns
      , part1Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test2 : Str
      , Machine.{ Machine, Name }
      , Graph.{ Graph }
      , Random
      ]

part1TestAns = 54
part1Ans = 547080

parse : Str -> Machine
parse = Machine.fromStr

# TODO: using parallelism, find a seed which finds the answer the quickest
part1 : Machine -> Nat
part1 = \machine ->
  Graph.mincutN machine 3
  |> Random.generateUntil Result.isOk
  |> Random.generate (Random.mkSeed 21)
  |> Result.map .partitions
  |> Result.withDefault []
  |> List.map List.len
  |> List.product
