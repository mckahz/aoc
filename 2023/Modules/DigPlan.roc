interface DigPlan
  exposes [DigPlan, fromStr, reinterpret]
  imports
    [ parser.Core.{ Parser, map, keep, skip, oneOf, sepBy, const, many }
    , parser.String.{ string, parseStr, digits, anyCodeunit}
    ]

DigPlan : List Step
Direction : [Left, Right, Up, Down]
Step :
  { direction : Direction
  , amount : I32
  , color : U64
  }

hexDigitsToU64 : List Str -> U64
hexDigitsToU64 = \hexDigits ->
  powersOf16 =
    List.range { start: At 0, end: Length (List.len hexDigits) }
    |> List.map \i -> 16 |> Num.powInt i

  hexDigits
  |> List.reverse
  |> List.map \digit ->
    when digit is
      "0" -> 0
      "1" -> 1
      "2" -> 2
      "3" -> 3
      "4" -> 4
      "5" -> 5
      "6" -> 6
      "7" -> 7
      "8" -> 8
      "9" -> 9
      "A" | "a" -> 10
      "B" | "b" -> 11
      "C" | "c" -> 12
      "D" | "d" -> 13
      "E" | "e" -> 14
      "F" | "f" -> 15
      _ -> 0
  |> List.map2 powersOf16 Num.mul
  |> List.sum

hash = "#"

fromStr : Str -> Result DigPlan [InvalidDigPlan]
fromStr = \str ->
  step =
    const \direction -> \amount -> \hexDigits ->
      { direction, amount: Num.toI32 amount, color: hexDigitsToU64 hexDigits }
    |> keep (
        oneOf
          [ string "R" |> map \_ -> Right
          , string "L" |> map \_ -> Left
          , string "U" |> map \_ -> Up
          , string "D" |> map \_ -> Down
          ]
    )
    |> skip (string " ")
    |> keep digits
    |> skip (string " (\(hash)")
    |> keep
        ( many
          ( "abcdefABCDEF0123456789"
            |> Str.graphemes
            |> List.map string
            |> oneOf
          )
        )
    |> skip (string ")")

  digPlan =
    step |> sepBy (string "\n")

  parseStr digPlan (Str.trim str)
  |> Result.mapErr \_ -> InvalidDigPlan

digitToHexString : Num * -> Str
digitToHexString = \a ->
  when a is
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    15 -> "f"
    _ -> crash "why did you pass something else you idiot"

toHexString : U64 -> Str
toHexString = \x ->
  List.range
    { start: At 0
    , end: Length 6
    }
  |> List.reverse
  |> List.map \i ->
      colValue = 16 |> Num.powInt i
      (x // colValue) % 16
  |> List.map digitToHexString
  |> Str.joinWith ""

reinterpret : DigPlan -> DigPlan
reinterpret = \digPlan ->
  digPlan
  |> List.map \step ->
      hexString = toHexString step.color

      amount =
        hexString
        |> Str.graphemes
        |> List.takeFirst 5
        |> hexDigitsToU64

      direction =
        hexString
        |> Str.graphemes
        |> List.takeLast 1
        |> hexDigitsToU64

      { step
      & amount : Num.toI32 amount
      , direction :
          when direction is
            0 -> Right
            1 -> Down
            2 -> Left
            3 -> Up
            _ -> crash "all colors should end with one of the above"
      }

