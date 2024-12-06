interface Day09.Solution
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
      ]

part1TestAns = 114
part1Ans = 1953784198
part2TestAns = 2
part2Ans = 957

OasisReport : List (List Nat)

parse : Str -> OasisReport
parse = \str -> str
    |> Str.split "\n"
    |> List.map \line ->
        line
        |> Str.split " "
        |> List.keepOks \nat -> Str.toNat nat
    |> List.dropLast 1

diffs : List Nat -> List (List Nat)
diffs = \history ->
    List.range { start: At 1, end: Before (List.len history) }
    |> List.walkUntil [history] \hists, i ->
        hist = hists |> List.last |> Result.withDefault []
        diff =
            List.map2 (hist |> List.dropFirst 1) hist \a, b ->
                a - b
        new = hists |> List.append diff
        if diff |> List.all \num -> num == 0 then
            Break new
        else
            Continue new

solution : (List Nat, List Nat -> List Nat), OasisReport -> Nat
solution = \extrapolate, reports ->
    reports
    |> List.map \report ->
        report
        |> diffs
        |> List.dropLast 1
        |> List.walkBackwards [0] extrapolate
        |> List.first
        |> Result.withDefault 0
    |> List.sum

extrapolateAfter : List Nat, List Nat -> List Nat
extrapolateAfter = \afterValues, diff ->
    when (afterValues, diff) is
        ([topMost, ..], [.., rightMost]) ->
            afterValues |> List.prepend (rightMost + topMost)

        _ -> crash "no values in lists"

part1 : OasisReport -> Nat
part1 = \reports ->
    solution extrapolateAfter reports

extrapolateBefore : List Nat, List Nat -> List Nat
extrapolateBefore = \beforeValues, diff ->
    when (beforeValues, diff) is
        ([topMost, ..], [leftMost, ..]) ->
            beforeValues |> List.prepend (leftMost - topMost)

        _ -> crash "no values in lists"

part2 : OasisReport -> Nat
part2 = \reports ->
    solution extrapolateBefore reports
