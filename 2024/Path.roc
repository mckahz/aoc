module [Path, shortest, allShortest]

import Map exposing [Map]

Path node : List node

bestNext : Set node, Dict node U64, (node -> U64) -> Result node [Missing]
bestNext = \available, distances, heuristic ->
    available
    |> Set.walk (Err Missing) \prev, curr ->
        curDist = distances |> Dict.get curr |> Result.withDefault Num.maxU64
        prevDist =
            when prev is
                Err Missing -> Num.maxU64
                Ok p -> distances |> Dict.get p |> Result.withDefault Num.maxU64

        when prev is
            Err Missing -> Ok curr
            Ok _ if curDist == Num.maxU64 -> prev
            Ok _ if prevDist == Num.maxU64 -> Ok curr
            Ok p ->
                comp = Num.compare (heuristic curr + curDist) (heuristic p + prevDist)
                when comp is
                    LT -> Ok curr
                    _ -> prev

shortest : node, (node -> Bool), (node -> List node), (node, node -> U64), (node -> U64) -> Path node where node implements Hash & Eq & Inspect
shortest = \start, isEnd, getNext, cost, heuristic ->
    distances = Dict.single start 0
    sources = Dict.single start start
    available = Set.single start
    shortestHelp distances sources available start isEnd getNext cost heuristic

reconstruct : Dict node node, List node, node -> Path node
reconstruct = \sources, path, end ->
    when sources |> Dict.get end is
        Err _ -> path
        Ok source if source == end -> (path |> List.prepend end)
        Ok source ->
            reconstruct sources (path |> List.prepend end) source

shortestHelp : Dict node U64, Dict node node, Set node, node, (node -> Bool), (node -> List node), (node, node -> U64), (node -> U64) -> Path node where node implements Hash & Eq & Inspect
shortestHelp = \distances, sources, available, start, isEnd, getNext, cost, heuristic ->
    if isEnd start then
        reconstruct sources [] start
    else
        additionalDistances =
            getNext start
            |> List.keepOks \next ->
                prevDist = distances |> Dict.get next |> Result.withDefault Num.maxU64
                curDist = distances |> Dict.get start |> Result.withDefault 0
                nextDist = curDist + cost start next

                if nextDist < prevDist then
                    Ok (next, nextDist)
                else
                    Err NotBetter
            |> Dict.fromList

        newSources =
            sources
            |> Dict.insertAll
                (
                    additionalDistances
                    |> Dict.map \_, _ -> start
                )

        newAvailable =
            available |> Set.remove start |> Set.union (additionalDistances |> Dict.keys |> Set.fromList)

        newDistances =
            distances
            |> Dict.insertAll additionalDistances

        bestNode = bestNext newAvailable newDistances heuristic

        when bestNode is
            Err Missing -> []
            Ok newBest ->
                shortestHelp newDistances newSources newAvailable newBest isEnd getNext cost heuristic

allShortest : node, (node -> Bool), (node -> List node), (node, node -> U64), (node -> U64) -> List (Path node) where node implements Hash & Eq & Inspect
allShortest = \start, isEnd, getNext, cost, heuristic ->
    distances = Dict.single start 0
    sources = Dict.single start [start]
    searched = Set.single start
    allShortestHelp distances sources searched start isEnd getNext cost heuristic

reconstructAll : Dict node (List node), List (List node), node -> List (Path node) where node implements Hash & Eq & Inspect
reconstructAll = \sources, paths, end ->
    when sources |> Dict.get end is
        Err _ -> paths
        Ok endSources ->
            newPaths =
                paths
                |> List.map \path ->
                    path |> List.prepend end
            if endSources == [end] then
                newPaths
            else
                endSources
                |> List.map \source ->
                    reconstructAll sources newPaths source
                |> List.join

allShortestHelp : Dict node U64, Dict node (List node), Set node, node, (node -> Bool), (node -> List node), (node, node -> U64), (node -> U64) -> List (Path node) where node implements Hash & Eq & Inspect
allShortestHelp = \distances, sources, available, start, isEnd, getNext, cost, heuristic ->
    if isEnd start then
        reconstructAll sources [[start]] start
    else
        additionalDistances =
            getNext start
            |> List.dropIf \next ->
                sources
                |> Dict.get start
                |> Result.withDefault []
                |> List.any \source -> source == next
            |> List.keepOks \next ->
                prevDist = distances |> Dict.get next |> Result.withDefault Num.maxU64
                curDist = distances |> Dict.get start |> Result.withDefault 0

                stepDist = cost start next

                nextDist = curDist + stepDist

                if nextDist <= prevDist then
                    Ok (next, curDist + stepDist)
                else
                    Err NotBetter
            |> Dict.fromList

        additionalSources =
            additionalDistances
            |> Dict.map \next, newDist ->
                prevDist = distances |> Dict.get next |> Result.withDefault Num.maxU64
                if newDist < prevDist then
                    [start]
                else if newDist == prevDist then
                    sources
                    |> Dict.get next
                    |> Result.withDefault []
                    |> List.append start
                else
                    sources |> Dict.get next |> Result.withDefault []

        newSources =
            sources |> Dict.insertAll additionalSources

        newAvailable =
            available
            |> Set.remove start
            |> Set.union
                (
                    additionalDistances
                    |> Dict.dropIf \(node, _) ->
                        Result.map2
                            (distances |> Dict.get node)
                            (additionalDistances |> Dict.get node)
                            \d1, d2 -> d1 == d2
                        |> Result.withDefault Bool.false
                    |> Dict.keys
                    |> Set.fromList
                )

        newDistances =
            distances |> Dict.insertAll additionalDistances

        newBestResult = bestNext newAvailable newDistances heuristic

        when newBestResult is
            Err _ -> crash "blah"
            Ok newBest ->
                allShortestHelp newDistances newSources newAvailable newBest isEnd getNext cost heuristic
