interface Day02.Solution
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
      ]


Color : [Red, Green, Blue]
Draw : List (Nat, Color)
Game :
  { id : Nat
  , draws : List Draw
  }

parse : Str -> List Game
parse = \str ->
  color = oneOf
    [ string "red" |> map \_ -> Red
    , string "green" |> map \_ -> Green
    , string "blue" |> map \_ -> Blue
    ]
  draw =
    sepBy
      ( const (\n -> \col -> (n, col))
        |> keep digits
        |> skip (string " ")
        |> keep color
      )
    (string ", ")
  draws = draw |> sepBy (string "; ")
  parser =
    const (\id -> \ds -> { id : id, draws: ds })
      |> skip (string "Game ")
      |> keep digits
      |> skip (string ": " )
      |> keep draws
  file = many (parser |> skip (string "\n"))
  parseStr file str
    |> Result.withDefault []

setMeetsRequirements : List (Nat, Color) -> Bool
setMeetsRequirements = \sets ->
  List.all sets \(n, color) ->
    when color is
      Red -> n <= 12
      Green -> n <= 13
      Blue -> n <= 14

gameMeetsRequirements : Game -> Bool
gameMeetsRequirements = \game ->
  List.all game.draws setMeetsRequirements

part1 : Str -> Nat
part1 = \str ->
  games = parse str
  games
    |> List.keepIf gameMeetsRequirements
    |> List.map \{ id } -> id
    |> List.sum

part2 : Str -> Nat
part2 = \str ->
  games = parse str
  games
    |> List.map \game ->
        # compiler bug without this type annotation
        max : Color -> Nat
        max = \col ->
          game.draws
            |> List.join
            |> List.keepIf \(_, c) -> c == col
            |> List.map .0
            |> List.walk 0 Num.max

        List.product
          [ max Red
          , max Green
          , max Blue
          ]
    |> List.sum
