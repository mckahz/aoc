interface Day01.Solution
    exposes
      [ parse
      , input, test1, test2
      , part1, part2
      ]
    imports
      [ "input.txt" as input : Str
      , "test1.txt" as test1 : Str
      , "test2.txt" as test2 : Str
      ]

parse : Str -> List Str
parse = \str -> str |> Str.trim |> Str.split "\n"

part1 : Str -> Nat
part1 = \str ->
    lines = parse str

    lines
        |> List.map \line ->
            nums = line
                |> Str.graphemes
                |> List.keepOks Str.toNat

            when nums is
                [a, .., b] -> 10 * a + b
                [c] -> 11 * c
                [] -> 0

        |> List.sum


wordOrDigitToNum : Str -> Result Nat [InvalidNumStr]
wordOrDigitToNum = \num ->
  when num is
    "zero" -> Ok 0
    "one" -> Ok 1
    "two" -> Ok 2
    "three" -> Ok 3
    "four" -> Ok 4
    "five" -> Ok 5
    "six" -> Ok 6
    "seven" -> Ok 7
    "eight" -> Ok 8
    "nine" -> Ok 9
    _ ->
      num |> Str.toNat

part2 : Str -> Nat
part2 = \str ->
  lines = parse str
  lines
    |> List.map \line ->
        graphemes = Str.graphemes line
        len = List.len graphemes
        lower <- List.joinMap (List.range { start: At 0, end: Before len})
        upper <- List.joinMap (List.range { start: At lower, end: Before len})
        substr = graphemes
          |> List.sublist { start: lower, len: (1 + upper - lower) }
          |> Str.joinWith ""

        when wordOrDigitToNum substr is
          Ok n -> [n]
          Err _ -> []
    |> List.map \nums ->
        when nums is
          [a, .., b] -> 10 * a + b
          [c] -> 11 * c
          [] -> 0
    |> List.sum
