app "day16"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports
      [ pf.Task
      , parser.Core.{ Parser, keep, skip, oneOf, sepBy, const, many }
      , parser.String.{ string, parseStr, parseStrPartial, digits, anyCodeunit }
      , Astar
      , "test.txt" as test : Str
      , "input.txt" as input : Str
      ]
    provides [main] to pf

Input : Dict Name Valve
Name : U16
Valve :
  { flowRate : U32
  , connections : List Name
  }

#expect
#  ans = solution1 (parse test)
#  ans == 1651
#expect
#  ans = solution1 (parse input)
#  ans == 1986
#expect
#  ans = solution2 (parse test)
#  ans == 1707
expect
  ans = solution2 (parse input)
  ans == 0

main = Task.ok {}

alpha =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  |> Str.graphemes
  |> List.map string
  |> oneOf

hash : Str -> Name
hash = \str ->
  const \l1 -> \l2 ->
    Num.shiftLeftBy (Num.toU16 l1) 8 + (Num.toU16 l2)
  |> keep anyCodeunit
  |> keep anyCodeunit
  |> parseStr str
  |> Result.withDefault 0

parse : Str -> Input
parse = \str ->
  name =
    many alpha
    |> Core.map \letters -> letters |> Str.joinWith ""
  line =
    const \n -> \flowRate -> \connections ->
      ( hash n
      , { flowRate :
            Num.toU32 flowRate
        , connections :
            connections
            |> List.map hash
        }
      )
    |> skip (string "Valve ")
    |> keep name
    |> skip (string " has flow rate=")
    |> keep digits
    |> skip (oneOf
        [ string "; tunnels lead to valves "
        , string "; tunnel leads to valve "
        ]
    )
    |> keep (name |> sepBy (string ", "))

  when parseStrPartial (line |> sepBy (string "\n")) str is
    Ok valves ->
      Dict.fromList valves.val
    Err e ->
      dbg e
      crash "invalid input"

Stratergy :
  { score : U32
  , opened : Set Name
  , timeLeft : U8
  , valve : Name
  }

getDistances : Dict Name Valve -> Dict (Name, Name) Nat
getDistances = \valves ->
  usefulValves = getUsefulValves valves

  namePairs =
    name1 <- List.joinMap (Dict.keys usefulValves)
    name2 <- List.joinMap (Dict.keys usefulValves)
    {} <- List.joinMap (if name1 == name2 then [] else [{}])
    [(name1, name2)]

  neighbors = \name ->
    valves
    |> Dict.get name
    |> Result.map .connections
    |> Result.withDefault []

  weight = \_, _ -> 1

  heuristic = \_ -> 0

  namePairs
  |> List.walk (Dict.empty {}) \paths, (name1, name2) ->
      if paths |> Dict.contains (name1, name2) then
        paths
      else
        shortest =
          Astar.findPath name1 name2 neighbors weight heuristic
          |> Result.withDefault []

        sublists =
          lower <- List.joinMap (List.range { start: At 0, end: Length (List.len shortest) })
          upper <- List.joinMap (List.range { start: After lower, end: Before (List.len shortest) })
          sublist = List.sublist shortest { start: lower, len: 1 + upper - lower }
          when sublist is
            [start, .., end] if !(paths |> Dict.contains (start, end)) -> [((start, end), sublist)]
            _ -> []

        sublists
        |> List.walk paths \oldPaths, (pair, path) -> oldPaths |> Dict.insert pair (List.len path - 1)

getUsefulValves : Dict Name Valve -> Dict Name Valve
getUsefulValves = \valves ->
  valves
  |> Dict.dropIf \(name, valve) ->
      valve.flowRate == 0 && name != (hash "AA")

findMostPressure : Dict Name Valve -> U32
findMostPressure = \valves ->
  usefulValves = getUsefulValves valves

  distances = getDistances valves

  dbg "discoverd distance between nodes"

  iter : Stratergy -> Stratergy
  iter = \strat ->
    openValves =
      usefulValves
      |> Dict.keys
      |> Set.fromList
      |> Set.difference strat.opened

    strats =
      openValves
      |> Set.map \openValve ->
        ( openValve
        , distances
            |> Dict.get (strat.valve, openValve)
            |> Result.withDefault (Num.toNat Num.maxU32)
        )
      |> Set.dropIf \(_, dist) -> dist + 1 >= Num.toNat strat.timeLeft
      |> Set.map \(valve, dist) ->
          valveFlowRate =
            valves
            |> Dict.get valve
            |> Result.map .flowRate
            |> Result.withDefault 0

          timeLeft = strat.timeLeft - Num.toU8 dist - 1
          score = strat.score + Num.toU32 timeLeft * valveFlowRate
          # do the janky shit again
          opened = strat.opened |> Set.toList |> List.append valve |> Set.fromList

          { score, opened, timeLeft, valve }

    strats
    |> Set.walk strat \bestSoFar, contendingStrat ->
        newStrat = iter contendingStrat
        if bestSoFar.score > newStrat.score then
          bestSoFar
        else
          newStrat

  aa = "AA"
  (iter
    { score : 0
    , opened : Set.empty {}
    , timeLeft : 30
    , valve : (hash aa)
    }
  ).score

solution1 : Input -> Nat
solution1 = \valves ->
  findMostPressure valves
  |> Num.toNat

developStrat : Dict Name Valve, Dict (Name, Name) Nat, Set Name, Stratergy -> Set Stratergy
developStrat = \valves, distances, openValves, strat ->
  openValves
  |> Set.map \openValve ->
      ( openValve
      , distances
          |> Dict.get (strat.valve, openValve)
          |> Result.withDefault (Num.toNat Num.maxU32)
      )
  |> Set.dropIf \(_, dist) -> dist + 1 >= Num.toNat strat.timeLeft
  |> Set.map \(valve, dist) ->
      valveFlowRate =
        valves
        |> Dict.get valve
        |> Result.map .flowRate
        |> Result.withDefault 0

      timeLeft = strat.timeLeft - Num.toU8 dist - 1
      score = strat.score + Num.toU32 timeLeft * valveFlowRate
      # do the janky shit again
      opened = strat.opened |> Set.toList |> List.append valve |> Set.fromList

      { score, opened, timeLeft, valve }

findMostPressureWithElephant : Dict Name Valve -> U32
findMostPressureWithElephant = \valves ->
  usefulValves = getUsefulValves valves

  distances = getDistances valves

  dbg "discoverd distance between nodes"

  iter : Stratergy, Stratergy -> (Stratergy, Stratergy)
  iter = \myStrat, elStrat ->
    dbg myStrat.score + elStrat.score

    openValves =
      usefulValves
      |> Dict.keys
      |> Set.fromList
      |> Set.difference myStrat.opened
      |> Set.difference elStrat.opened

    stratPairs =
      empty = Set.empty {}
      es <- Set.joinMap (developStrat valves distances openValves elStrat)
      ms <- Set.joinMap (developStrat valves distances openValves myStrat)
      when {} is
        _ if es.valve == ms.valve -> empty
        _ -> Set.single (ms, es)

    stratPairs
    |> Set.walk (myStrat, elStrat) \(bestMS, bestES), (ms, es) ->
        (newMS, newES) = iter ms es
        if bestMS.score + bestES.score > newMS.score + newES.score then
          (bestMS, bestES)
        else
          (newMS, newES)

  (ms1, es1) =
    iter
      { score : 0
      , opened : Set.empty {}
      , timeLeft : 26
      , valve : (hash "AA")
      }
      { score : 0
      , opened : Set.empty {}
      , timeLeft : 26
      , valve : (hash "AA")
      }
  ms1.score + es1.score

solution2 : Input -> Nat
solution2 = \valves ->
  findMostPressureWithElephant valves
  |> Num.toNat
