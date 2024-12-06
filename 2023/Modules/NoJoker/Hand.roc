interface Hand
  exposes [Hand, make, compare]
  imports
    [ Card.{ Card }
    , Type.{ Type }
    ]

Hand :
  { cards : (Card, Card, Card, Card, Card)
  , type : Type
  }

make : (Card, Card, Card, Card, Card) -> Hand
make = \cards ->
  { cards, type: Type.fromCards cards }

compare : Hand, Hand -> [LT, GT, EQ]
compare = \h1, h2 ->
  toList = \cards -> [ .0, .1, .2, .3, .4 ] |> List.map \nth -> nth cards
  typeComp = Type.compare h1.type h2.type
  if typeComp != EQ then
    typeComp
  else
    (List.map2 (toList h1.cards) (toList h2.cards) \a, b -> (a, b))
      |> List.map \(c1, c2) ->
          Card.compare c1 c2
      |> List.findFirst \comp -> comp != EQ
      |> Result.withDefault EQ
