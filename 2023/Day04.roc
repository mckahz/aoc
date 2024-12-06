interface Day04.Solution
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

Game : {
  id: Nat,
  winningNumbers: List Nat,
  yourNumbers: List Nat,
}

parse : Str -> List Game
parse = \str ->
  game =
    const \id -> \winning -> \yours ->
      { id: id, winningNumbers: winning, yourNumbers: yours }
    |> skip (string "Card") |> skip (many (string " "))
    |> keep digits |> skip (string ":") |> skip (many (string " "))
    |> keep (digits |> sepBy (many (string " "))) |> skip (many (string " "))
    |> skip (many (string " ")) |> skip (string "|") |> skip (many (string " "))
    |> keep (digits |> sepBy (many (string " ")))

  str
    |> Str.split "\n"
    |> List.dropLast 1
    |> List.keepOks \line ->
        parseStr game line

countMatches : Game -> Nat
countMatches = \game ->
  Set.intersection
    (Set.fromList game.winningNumbers)
    (Set.fromList game.yourNumbers)
  |> Set.len

part1 : Str -> Nat
part1 = \s ->
  games = parse s
  games
    |> List.map \game ->
        when countMatches game is
          0 -> 0
          matches -> 2 |> Num.powInt (matches - 1)
    |> List.sum

countGames : Dict Nat { game: Game, count: Nat }, Nat, Nat -> Nat
countGames = \games, count, playing ->
  when games |> Dict.get playing is
    Err _ -> count
    Ok game ->
      gamesWithResetCount =
        Dict.update games playing \v ->
          when v is
            Present gac -> Present { gac & count : 0 }
            Missing -> Missing

      gamesWithNewCounts =
        List.range { start: After playing, end: At (playing + countMatches game.game) }
          |> List.walk gamesWithResetCount \gs, i ->
              Dict.update gs i \v ->
                when v is
                  Present gac -> Present { gac & count : gac.count + game.count }
                  Missing -> Missing

      countGames gamesWithNewCounts (count + game.count) (playing + 1)

part2 : Str -> Nat
part2 = \s ->
  games = parse s
  List.map games (\game -> ( game.id, { game: game, count: 1 } ))
    |> Dict.fromList
    |> countGames 0 1
