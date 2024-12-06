interface Type
  exposes [Type, fromCards, compare]
  imports [Card.{ Card }, Util.{ unique }]

Type :
  [ FiveOfAKind
  , FourOfAKind
  , FullHouse
  , ThreeOfAKind
  , TwoPair
  , OnePair
  , HighCard
  ]

compare : Type, Type -> [LT, EQ, GT]
compare = \t1, t2 ->
  order =
    [ FiveOfAKind
    , FourOfAKind
    , FullHouse
    , ThreeOfAKind
    , TwoPair
    , OnePair
    , HighCard
    ]
  t1i = order |> List.findFirstIndex \type -> type == t1
  t2i = order |> List.findFirstIndex \type -> type == t2
  when (t1i, t2i) is
    (Ok i1, Ok i2) -> Num.compare i2 i1
    _ -> EQ


fromCards : (Card, Card, Card, Card, Card) -> Type
fromCards = \(a, b, c, d, e) ->
  cards : List Card
  cards = [a, b, c, d, e]

  cardCounts : List Nat
  cardCounts =
    cards
      |> List.map \card -> (card, cards |> List.countIf \otherCard -> otherCard == card)
      |> unique
      |> List.map .1
      |> List.sortDesc

  when cardCounts is
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    [3, 1, 1] -> ThreeOfAKind
    [2, 2, 1] -> TwoPair
    [2, 1, 1, 1] -> OnePair
    _ -> HighCard
