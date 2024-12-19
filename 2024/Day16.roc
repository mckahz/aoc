module [example1, example2, myInput, part1, part2]

import "inputs/day16-example1.txt" as example1 : List U8
import "inputs/day16-example2.txt" as example2 : List U8
import "inputs/day16-input.txt" as myInput : List U8

import Linear exposing [down, left, up, right]
import Map exposing [Pos, Dir]
import Path

Map : Map.Map [Wall]
Path : Path.Path Node
Node : {
    pos : Pos,
    dir : Dir,
}

parse = \input ->
    map =
        input
        |> Map.parse \c ->
            when c is
                '#' -> Cell Wall
                _ -> Empty

    start =
        input
        |> Map.parse \c ->
            when c is
                'S' -> Cell Start
                _ -> Empty
        |> Map.find Start
        |> Result.withDefault Linear.zero

    end =
        input
        |> Map.parse \c ->
            when c is
                'E' -> Cell End
                _ -> Empty
        |> Map.find End
        |> Result.withDefault Linear.zero

    (map, start, end)

getScore : Path -> U32
getScore = \path ->
    when path is
        [] -> 0
        [start, .. as rest] ->
            rest
            |> List.walk { score: 0, prev: start } \state, next ->
                score =
                    if state.prev.dir != next.dir then
                        state.score + 1000 + 1
                    else
                        state.score + 1
                { score, prev: next }
            |> .score

part1 = \input ->
    (map, start, end) = parse input
    isEnd = \node -> node.pos == end
    getNext = \node ->
        [
            { pos: left node.pos, dir: Left },
            { pos: right node.pos, dir: Right },
            { pos: up node.pos, dir: Up },
            { pos: down node.pos, dir: Down },
        ]
        |> List.dropIf \next -> map |> Map.has Wall next.pos
    cost = \n1, n2 ->
        if n1.dir == n2.dir then
            1
        else
            1001
    heuristic = \_ -> 0
    Path.shortest { pos: start, dir: Right } isEnd getNext cost heuristic
    |> getScore

part2 = \input ->
    (map, start, end) = parse input

    isEnd = \node -> node.pos == end
    getNext = \node ->
        [
            { pos: left node.pos, dir: Left },
            { pos: right node.pos, dir: Right },
            { pos: up node.pos, dir: Up },
            { pos: down node.pos, dir: Down },
        ]
        |> List.dropIf \next -> map |> Map.has Wall next.pos
    cost = \n1, n2 ->
        if n1.dir == n2.dir then
            1
        else
            1001

    Path.allShortest { pos: start, dir: Right } isEnd getNext cost \_ -> 0
    |> Set.fromList
    |> Set.joinMap \path ->
        path
        |> List.map .pos
        |> Set.fromList
    |> Set.len
