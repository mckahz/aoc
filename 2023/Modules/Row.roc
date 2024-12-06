interface Row
  exposes [Row, fromStr, toStr, countArrangements, unfold]
  imports 
    [ Spring.{ Spring }
    , Cache
    ]

Row :
  { springs: List Spring
  , groups: List Nat
  }

fromStr : Str -> Result Row [WrongNumberOfSpaces]
fromStr = \str ->
  when str |> Str.split " " is
    [springStr, numStr] ->
      springs =
        springStr
          |> Str.graphemes
          |> List.keepOks Spring.fromStr
      groups =
        numStr
          |> Str.split ","
          |> List.keepOks Str.toNat
      Ok { springs, groups }
    _ -> Err WrongNumberOfSpaces

toStr : Row -> Str
toStr = \row ->
  springs =
    row.springs
      |> List.map Spring.toStr
      |> Str.joinWith ""

  groups =
    row.groups
      |> List.map Num.toStr
      |> List.intersperse ","
      |> Str.joinWith ""

  [springs, "  (", groups, ")"] |> Str.joinWith ""

unfold : Row -> Row
unfold = \row ->
  n = 5
  { row
  & springs:
      row.springs
        |> List.repeat n
        |> List.intersperse [Unknown]
        |> List.join
  , groups:
      row.groups
        |> List.repeat n
        |> List.join
  }

Cache : Dict Row Nat

countArrangements : Row -> Nat
countArrangements = \r ->
  iter : Row, Cache -> (Nat, Cache)
  iter = \row, cache ->
    when (cache |> Dict.get row, row.groups) is
      (Ok count, _) ->
        (count, cache)
      (Err _, []) ->
        if row.springs |> List.all \spring -> spring != Damaged then
          (1, cache |> Dict.insert row 1)
        else
          (0, cache |> Dict.insert row 0)
      (Err _, [firstGroup, .. as remainingGroups]) ->
        # Add the sizes of the remaining groups, plus 1 extra for each gap that must exist
        remainingSpringSpaces = List.sum remainingGroups + List.len remainingGroups
        List.range
          { start: At 0
          , end: Length (List.len row.springs - remainingSpringSpaces - firstGroup + 1)
          }
        |> List.map \i ->
            [ List.repeat Operational i
            , List.repeat Damaged firstGroup
            , [Operational]
            ]
            |> List.join
        |> List.keepIf \possibleSprings ->
            List.map2 row.springs possibleSprings Pair
              |> List.all \(Pair spring possibleSpring) ->
                  spring == possibleSpring || spring == Unknown
        |> List.walk (0, cache) \(count, c), possibleSprings ->
            (arragements, newCache) =
              iter
                { row
                & springs : row.springs |> List.dropFirst (List.len possibleSprings)
                , groups : remainingGroups
                }
                c
            (count + arragements, newCache)
        |> \(count, c) -> (count, c |> Dict.insert row count)
      (Err _, _) -> crash "compiler bug"
  iter r (Dict.empty {}) |> .0

# countArrangements with caching
countArrangementsWithCachingMonad : Row -> Nat
countArrangementsWithCachingMonad = \r ->
  iter : Row -> Cache.Cache Nat (Dict Row Nat)
  iter = \row ->
    cache <- Cache.get |> Cache.andThen
    dbg row |> toStr
    when (cache |> Dict.get row, row.groups) is
      (Ok count, _) ->
        Cache.ignore count
      # Base case
      (_, []) ->
        if row.springs |> List.all \spring -> spring != Damaged then
          Cache.ignore 1
            |> Cache.updateCache (\c -> c |> Dict.insert row 1)
        else
          Cache.ignore 0
            |> Cache.updateCache (\c -> c |> Dict.insert row 0)
      # Recursive case
      (_, [firstGroup, .. as remainingGroups]) ->
        # Add the sizes of the remaining groups, plus 1 extra for each gap that must exist
        remainingSpringSpaces = List.sum remainingGroups + List.len remainingGroups
        List.range
          { start: At 0
          , end: Length (List.len row.springs - remainingSpringSpaces - firstGroup + 1)
          }
        |> List.map \i ->
            [ List.repeat Operational i
            , List.repeat Damaged firstGroup
            , [Operational]
            ]
            |> List.join
        |> List.keepIf \possibleSprings ->
            List.map2 row.springs possibleSprings Pair
              |> List.all \(Pair spring possibleSpring) ->
                  spring == possibleSpring || spring == Unknown
        |> List.map \possibleSprings ->
            { row
            & springs : row.springs |> List.dropFirst (List.len possibleSprings)
            , groups : remainingGroups
            }
        |> List.map \newRow -> \_ -> iter newRow
        |> Cache.sequence
        |> Cache.andThen \counts ->
            Cache.ignore (List.sum counts)
              |> Cache.updateCache (\c -> c |> Dict.insert row 0) # count
      (Err _, _) -> crash "all list cases are covered by either error or ok so this is a compiler bug"
  Cache.run (iter r) (Dict.empty {})
