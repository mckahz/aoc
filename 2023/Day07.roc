interface Day07.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , parser.Core.{ Parser, map, keep, skip, oneOf, sepBy, const, many }
      , parser.String.{ string, parseStr, digits }
      , Day07.Joker.Hand
      , Day07.Joker.Card
      , Day07.Joker.Type
      , Day07.Hand
      , Day07.Card
      , Day07.Type
      ]

part1TestAns = 6440
part1Ans = 248569531
part2TestAns = 5905
part2Ans = 250382098

Input : List ((Str, Str, Str, Str, Str), Nat)

parse : Str -> Input
parse = \str ->
  card =
    oneOf
      [ string "A"
      , string "K"
      , string "Q"
      , string "J"
      , string "T"
      , string "9"
      , string "8"
      , string "7"
      , string "6"
      , string "5"
      , string "4"
      , string "3"
      , string "2"
      ]

  hand =
    const \a -> \b -> \c -> \d -> \e ->
      (a, b, c, d, e)
    |> keep card
    |> keep card
    |> keep card
    |> keep card
    |> keep card

  line =
    const \h -> \bid ->
      (h, bid)
    |> keep hand
    |> skip (string " ")
    |> keep digits

  game =
    line |> sepBy (string "\n") |> skip (string "\n")

  when parseStr game str is
    Ok i -> i
    Err _ -> crash "invalid input"


part1 : Input -> Nat
part1 = \cardsAndBids ->
  cardsAndBids
    |> List.map \((a, b, c, d, e), bids) ->
        ( Day07.Hand.make
          ( Day07.Card.fromStr a |> Result.withDefault Ace
          , Day07.Card.fromStr b |> Result.withDefault Ace
          , Day07.Card.fromStr c |> Result.withDefault Ace
          , Day07.Card.fromStr d |> Result.withDefault Ace
          , Day07.Card.fromStr e |> Result.withDefault Ace
          ), bids)
    |> List.sortWith \(h1, _), (h2, _) -> Day07.Hand.compare h1 h2
    |> List.map .1
    |> List.map2 (List.range { start: At 1, end: Length (List.len cardsAndBids) })
        \bid, rank -> bid * rank
    |> List.sum

part2 : Input -> Nat
part2 = \cardsAndBids ->
  cardsAndBids
    |> List.map \((a, b, c, d, e), bids) -> 
        ( Day07.Joker.Hand.make
          ( Day07.Joker.Card.fromStr a |> Result.withDefault Ace
          , Day07.Joker.Card.fromStr b |> Result.withDefault Ace
          , Day07.Joker.Card.fromStr c |> Result.withDefault Ace
          , Day07.Joker.Card.fromStr d |> Result.withDefault Ace
          , Day07.Joker.Card.fromStr e |> Result.withDefault Ace
          ), bids)
    |> List.sortWith \(h1, _), (h2, _) -> Day07.Joker.Hand.compare h1 h2
    |> List.map .1
    |> List.map2 (List.range { start: At 1, end: Length (List.len cardsAndBids) })
        \bid, rank -> bid * rank
    |> List.sum
