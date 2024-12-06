interface Joker.Card
  exposes [Card, all, nonJoker, fromStr, toStr, compare]
  imports []

Card : [Ace, King, Queen, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Joker]

all : List Card
all = [Ace, King, Queen, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Joker]

nonJoker : List Card
nonJoker = [Ace, King, Queen, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]

fromStr = \str ->
  when str is
    "A" -> Ok Ace
    "K" -> Ok King
    "Q" -> Ok Queen
    "J" -> Ok Joker
    "T" -> Ok Ten
    "9" -> Ok Nine
    "8" -> Ok Eight
    "7" -> Ok Seven
    "6" -> Ok Six
    "5" -> Ok Five
    "4" -> Ok Four
    "3" -> Ok Three
    "2" -> Ok Two
    _ -> Err NotACard

toStr = \str ->
  when str is
    Ace -> "A"
    King -> "K"
    Queen -> "Q"
    Joker -> "J"
    Ten -> "T"
    Nine -> "9"
    Eight -> "8"
    Seven -> "7"
    Six -> "6"
    Five -> "5"
    Four -> "4"
    Three -> "3"
    Two -> "2"

compare : Card, Card -> [LT, GT, EQ]
compare = \c1, c2 ->
  c1i = all |> List.findFirstIndex \card -> card == c1
  c2i = all |> List.findFirstIndex \card -> card == c2
  when (c1i, c2i) is
    (Ok i1, Ok i2) -> Num.compare i2 i1
    _ -> EQ
