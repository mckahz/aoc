module [example, myInput, part1, part2]

import "inputs/day19-example.txt" as example : List U8
import "inputs/day19-input.txt" as myInput : List U8

Color : U8
Towel : List Color

parse = \input ->
    when input |> List.splitOnList ['\n', '\n'] is
        [] | [_] | [_, _, _, ..] -> crash "invalid input"
        [towels, designs] ->
            (
                towels |> List.splitOnList [',', ' '],
                designs
                |> List.splitOn '\n'
                |> List.dropLast 1,
            )

canBuild : List Towel, Towel -> Bool
canBuild = \towels, design ->
    if design |> List.isEmpty then
        Bool.true
    else
        starts =
            towels
            |> List.keepIf \towel ->
                design |> List.startsWith towel
        ends =
            towels
            |> List.keepIf \towel ->
                design |> List.endsWith towel
        starts
        |> List.any \start ->
            ends
            |> List.any \end ->
                canBuild
                    towels
                    (
                        design
                        |> List.dropFirst (List.len start)
                        |> List.dropLast (List.len end)
                    )

part1 = \input ->
    (towels, designs) = parse input
    designs
    |> List.countIf \design ->
        towels |> canBuild design

waysToBuild = \towels, design ->
    waysToBuildHelp towels design (Dict.empty {})
    |> .0

waysToBuildHelp = \towels, design, cache ->
    if cache |> Dict.contains design then
        total = cache |> Dict.get design |> Result.withDefault 0
        (total, cache)
    else if design |> List.isEmpty then
        (1, cache)
    else
        starts =
            towels
            |> List.keepIf \towel ->
                design |> List.startsWith towel

        (t, c) =
            starts
            |> List.walk (0, cache) \(total, oldCache), start ->
                restDesign =
                    design |> List.dropFirst (List.len start)

                if towels |> canBuild restDesign then
                    (additional, newCache) = towels |> waysToBuildHelp restDesign oldCache
                    (total + additional, newCache)
                else
                    (total, oldCache)

        (t, c |> Dict.insert design t)

part2 = \input ->
    (towels, designs) = parse input
    designs
    |> List.keepIf \design ->
        towels |> canBuild design
    |> List.mapWithIndex \d, i -> (d, i)
    |> List.map \(design, i) ->
        dbg i
        towels
        |> waysToBuild design
    |> List.sum
    |> dbg
