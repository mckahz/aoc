interface Machine
  exposes [Machine, Name, fromStr]
  imports
    [ Graph.{ Graph }
    ]

Name : Str
Machine : Graph Name

fromStr : Str -> Machine
fromStr = \str ->
  edges =
    str
    |> Str.split "\n"
    |> List.dropLast 1
    |> List.keepOks \line ->
        when line |> Str.split ":" is
          [start, others] ->
            others
            |> Str.trim
            |> Str.split " "
            |> List.map \other -> (start, other)
            |> Ok
          _ -> Err WrongColonCount
    |> List.join

  verts =
    edges
    |> List.joinMap \(start, end) ->
        [start, end]
    |> Set.fromList
    |> Set.toList

  { verts, edges }
