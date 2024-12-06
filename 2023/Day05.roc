interface Day05.Solution
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

Mapping : (Nat, Nat, Nat)
Almanac :
  { seeds: List Nat
  , seedToSoil : List Mapping
  , soilToFertilizer : List Mapping
  , fertilizerToWater : List Mapping
  , waterToLight : List Mapping
  , lightToTemp : List Mapping
  , tempToHumidity : List Mapping
  , humidityToLocation : List Mapping
  }

parse : Str -> Almanac
parse = \str ->
  listToMapping = \ints ->
    ( when ints is
        [destStart, sourceStart, len] ->
          Ok (destStart, sourceStart, len)
        _ ->
          Err WrongArgumentCount
    )

  mapping =
    digits
      |> sepBy (string " ")
      |> map listToMapping

  mappings = mapping |> sepBy (string "\n") |> map (\ms -> List.walk ms [] List.appendIfOk)

  parser =
    const (
      \ seeds ->
        \ seedToSoil ->
          \ soilToFertilizer ->
            \ fertilizerToWater ->
              \ waterToLight ->
                \ lightToTemp ->
                  \ tempToHumidity ->
                    \ humidityToLocation ->
                      { seeds
                      , seedToSoil
                      , soilToFertilizer
                      , fertilizerToWater
                      , waterToLight
                      , lightToTemp
                      , tempToHumidity
                      , humidityToLocation
                      }
      )
    |> skip (string "seeds: ")
    |> keep (digits |> sepBy (string " "))
    |> skip (string "\n\nseed-to-soil map:\n")
    |> keep mappings
    |> skip (string "soil-to-fertilizer map:\n")
    |> keep mappings
    |> skip (string "fertilizer-to-water map:\n")
    |> keep mappings
    |> skip (string "water-to-light map:\n")
    |> keep mappings
    |> skip (string "light-to-temperature map:\n")
    |> keep mappings
    |> skip (string "temperature-to-humidity map:\n")
    |> keep mappings
    |> skip (string "humidity-to-location map:\n")
    |> keep mappings

  when parseStr parser str is
    Ok i -> i
    Err _ -> crash ""

seedToLocation : Almanac, Nat -> Nat
seedToLocation = \almanac, seed ->
  [ .seedToSoil
  , .soilToFertilizer
  , .fertilizerToWater
  , .waterToLight
  , .lightToTemp
  , .tempToHumidity
  , .humidityToLocation
  ]
  |> List.map \mappings -> mappings almanac
  |> List.walk seed \i, mappings ->
      mappings
        |> List.findFirst \(_, source, len) -> source <= i && i <= source + len
        |> Result.map \(dest, source, _) -> dest + (i - source)
        |> Result.withDefault i

part1 : Str -> Nat
part1 = \s ->
  almanac = parse s
  almanac.seeds
    |> List.map (\seed -> seedToLocation almanac seed)
    |> List.min
    |> Result.withDefault 0

Range : (Nat, Nat)

pairs : List a -> List (a, a)
pairs = \list ->
  when (list |> List.first, list |> List.dropFirst 1 |> List.first) is
    (Ok first, Ok second) -> pairs (list |> List.dropFirst 2) |> List.prepend (first, second)
    _ -> []

without : Range, Range -> List Range
without = \(srcStart, srcLen), (rmStart, rmLen) ->
  [ if srcStart < rmStart then 
      end = Num.min (srcStart + srcLen - 1) (rmStart - 1)
      [(srcStart, 1 + end - srcStart)]
    else
      []
  , if srcStart + srcLen > rmStart + rmLen then
      start = Num.max (rmStart + rmLen) srcStart
      [(start, srcStart + srcLen - start)]
    else
      []
  ]
  |> List.join

overlap : Range, Range -> Bool
overlap = \(start1, len1), (start2, len2) ->
  start1 < start2 + len2 && start2 < start1 + len1

intersection : Range, Range -> List Range
intersection = \(start1, len1), (start2, len2) ->
  if overlap (start1, len1) (start2, len2) then
    start = Num.max start1 start2
    end = Num.min (start1 + len1) (start2 + len2) - 1
    [(start, 1 + end - start)]
  else
    []

srcRange : Mapping -> Range
srcRange = \(_, srcStart, size) ->
  (srcStart, size)

mapIncluded : Range, Mapping -> Range
mapIncluded = \(rangeStart, rangeLen), (destStart, srcStart, _) ->
  (rangeStart + destStart - srcStart, rangeLen)

indexRange : Range, List Mapping -> List Range
indexRange = \idx, mappings ->
  mappingRanges = mappings |> List.map srcRange

  rangesWithMapping =
    mappingRange <- List.joinMap mappingRanges
    intersection idx mappingRange

  rangesWithoutMapping =
    ranges, mappingRange <- List.walk mappingRanges [idx]
    range <- List.joinMap ranges
    range |> without mappingRange

  mappedRanges =
    range <- List.map rangesWithMapping
    mapping =
      mappings
        |> List.findFirst \m -> (srcRange m) |> overlap range
        |> Result.withDefault (0, 0, 0)
    mapIncluded range mapping

  List.concat mappedRanges rangesWithoutMapping

part2 : Str -> Nat
part2 = \s ->
  almanac = parse s
  mapRanges : List (Nat, Nat) -> List (Nat, Nat)
  mapRanges = \seedRanges ->
    [ .seedToSoil
    , .soilToFertilizer
    , .fertilizerToWater
    , .waterToLight
    , .lightToTemp
    , .tempToHumidity
    , .humidityToLocation
    ]
    |> List.map \mappings -> mappings almanac
    |> List.walk seedRanges \ranges, mappings ->
        range <- List.joinMap ranges
        indexRange range mappings

  mapRanges (pairs almanac.seeds)
    |> List.map .0
    |> List.min
    |> Result.withDefault 0
