interface Day20.Solution
    exposes
      [ parse
      , input, test1, test2
      , part1, part2
      , part1Test1Ans, part1Test2Ans
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test1.txt" as test1 : Str
      , "test2.txt" as test2 : Str
      , Path
      ]

part1Test1Ans = 32000000
part1Test2Ans = 11687500
part1Ans = 739960225
part2Ans = 231897990075517

parse : Str -> Automaton
parse = \str ->
  module : Parser (List U8) Module
  module =
    oneOf
      [ const \names ->
          { name: "broadcaster"
          , type: Broadcast
          , outputs: names
          }
        |> skip (string "broadcaster -> ")
        |> keep (alphas |> sepBy (string ", "))

      , const \name -> \names ->
          { name
          , type: FlipFlop Off
          , outputs: names
          }
        |> skip (string "%")
        |> keep alphas
        |> skip (string " -> ")
        |> keep (alphas |> sepBy (string ", "))

      , const \name -> \names ->
          { name
          , type: Conjunction (Dict.empty {})
          , outputs: names
          }
        |> skip (string "&")
        |> keep alphas
        |> skip (string " -> ")
        |> keep (alphas |> sepBy (string ", "))
      ]

  automaton =
    module
    |> sepBy (string "\n")
    |> skip (string "\n")
    |> map populateConjunctionInputs

  when parseStr automaton str is
    Ok i -> i
    Err e ->
      dbg e
      crash "invalid input"

populateConjunctionInputs : Automaton -> Automaton
populateConjunctionInputs = \automaton ->
  List.map automaton \module ->
    name = module.name
    { module
    & type:
        if !(module |> Module.isConjunction) then
          module.type
        else
          automaton
          |> List.keepIf \inputModule ->
              inputModule.outputs |> List.contains name
          |> List.map \inputModule -> (inputModule.name, Low)
          |> Dict.fromList
          |> Conjunction
    }

part1 : Automaton -> Nat
part1 = \automaton ->
  sent = automaton |> Automaton.pushButtonN 1000
  sent.low * sent.high

part2 : Automaton -> Nat
part2 = \automaton ->
  automaton
  |> Automaton.findSubautomata
  |> List.map Automaton.buttonPressesTilConjunctionFires
  |> List.walk 1 Math.lcm
