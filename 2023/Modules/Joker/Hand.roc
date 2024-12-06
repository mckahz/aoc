interface Joker.Hand
  exposes [Hand, make, compare]
  imports
    [ Joker.Card.{ Card }
    , Joker.Type.{ Type }
    ]

Hand :
  { cards : (Card, Card, Card, Card, Card)
  , substituted : (Card, Card, Card, Card, Card)
  , type : Type
  }

tupleToList : (Card, Card, Card, Card, Card) -> List Card
tupleToList = \(a, b, c, d, e) -> [a, b, c, d, e]

listToTuple : List Card -> (Card, Card, Card, Card, Card)
listToTuple = \cards -> when cards is
  [a, b, c, d, e] -> (a, b, c, d, e)
  _ -> crash "invalid cards list"

substituteJokers : List Card, List Card -> List Card
substituteJokers = \cards, jokers ->
  when jokers is
    [] ->
      cards
    [j, ..] ->
      idx = cards
          |> List.findFirstIndex \card -> card == Joker
          |> Result.withDefault 0
      substituteJokers
        (cards |> List.update idx \_ -> j)
        (jokers |> List.dropFirst 1)

make : (Card, Card, Card, Card, Card) -> Hand
make = \cards ->
  cardsList = tupleToList cards

  jokerCount = cardsList |> List.countIf \card -> card == Joker

  when jokerCount is
    0 -> { cards: cards, substituted: cards, type: Joker.Type.fromCards cards }
    5 ->
      { cards
      , substituted: (Ace, Ace, Ace, Ace, Ace)
      , type: FiveOfAKind
      }
    4 ->
      other =
        cardsList
          |> List.findFirst \card -> card != Joker
          |> Result.withDefault Joker
      { cards
      , substituted: (other, other, other, other, other)
      , type: FiveOfAKind
      }
    _ ->
      possibleJokerSubstitutions =
        List.range { start: At 0, end: Length jokerCount }
          |> List.walk [[]] \possibilities, _ ->
              possibility <- List.joinMap possibilities
              cardToAppend <- List.joinMap Joker.Card.nonJoker
              hasSmallerCards = List.any possibility \card -> 
                Joker.Card.compare card cardToAppend == LT
              {} <- List.joinMap (if hasSmallerCards then [] else [{}])
              [List.append possibility cardToAppend]

      bestHand =
        possibleJokerSubstitutions
          |> List.map \jokers ->
              cardsList |> substituteJokers jokers
          |> List.map listToTuple
          |> List.map \substituted -> { cards, substituted, type: Joker.Type.fromCards substituted }
          |> List.sortWith compare
          |> List.last

      when bestHand is
        Ok hand -> hand
        Err _ -> crash "no hands available"

compare : Hand, Hand -> [LT, GT, EQ]
compare = \h1, h2 ->
  typeComp = Joker.Type.compare h1.type h2.type
  if typeComp != EQ then
    typeComp
  else
    List.map2 (tupleToList h1.cards) (tupleToList h2.cards) \c1, c2 ->
          Joker.Card.compare c1 c2
      |> List.findFirst \comp -> comp != EQ
      |> Result.withDefault EQ
