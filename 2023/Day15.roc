interface Day15.Solution
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

part1TestAns = 1320
part1Ans = 497373
part2TestAns = 145
part2Ans = 259356

parse : Str -> List Str
parse = \str ->
  str
  |> Str.split ","
  |> List.map Str.trim

graphemes : List Str
graphemes = "123456789abcdfghjklmnopqrstvxz=-HAS" |> Str.graphemes

codes : List Nat
codes = [49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 102, 103, 104, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 118, 120, 122, 61, 45, 72, 65, 83]

ascii : Str -> Nat
ascii = \str ->
  List.map2 graphemes codes \grapheme, code -> (grapheme, code)
  |> Dict.fromList
  |> Dict.get str
  |> Result.withDefault 0

hash : Str -> Nat
hash = \str ->
  str
  |> Str.graphemes
  |> List.map ascii
  |> List.walk 0 \current, code ->
      (current + code) * 17 |> Num.rem 256

part1 : List Str -> Nat
part1 = \strings ->
  strings
  |> List.map hash
  |> List.sum

Step :
  { label : Str
  , operation : [Equals Nat, Minus]
  }

toStep : Str -> Step
toStep = \str ->
  when str |> Str.graphemes is
    [.. as label, "=", n] ->
      { label: label |> Str.joinWith ""
      , operation: Equals (n |> Str.toNat |> Result.withDefault 0)
      }
    [.. as label, "-"] ->
      { label: label |> Str.joinWith ""
      , operation: Minus
      }
    _ -> crash "all the patterns should adhere"

part2 : List Str -> Nat
part2 = \strings ->
  strings
  |> List.map toStep
  |> List.walk (Dict.empty {}) \boxes, step ->
      boxes
      |> Dict.update (hash step.label) \v ->
          when (v, step.operation) is
            (Missing, Minus) -> Missing
            (Present lenses, Minus) ->
              box = lenses |> List.dropIf \(label, _) -> label == step.label
              when box is
                [] -> Missing
                _ -> Present box

            (Missing, Equals n) -> Present [(step.label, n)]
            (Present lenses, Equals n) ->
              Present (
                lenses
                |> List.findFirstIndex \(label, _) ->
                    label == step.label
                |> Result.map \idx ->
                    List.update lenses idx \_ -> (step.label, n)
                |> Result.withDefault (lenses |> List.append (step.label, n))
              )
  |> Dict.toList
  |> List.joinMap \(box, lenses) ->
      lenses |> List.mapWithIndex \(_, focalLength), i -> (box, i, focalLength)
  |> List.map \(box, i, focalLength) ->
      (box + 1) * (i + 1) * focalLength
  |> List.sum
