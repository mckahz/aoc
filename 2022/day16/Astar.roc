interface Astar
  exposes [findPath]
  imports []

Tree node :
  [ Tree node (List (Tree node)) ]

findPath : node, node, (node -> List node), (node, node -> Nat), (node -> Nat) -> Result (List node) [NoPathExists]
  where node implements Hash & Eq & Inspect
findPath = \start, end, neighbors, weight, heuristic ->
  iter : Set node, Dict node node, Dict node Nat, Dict node Nat -> Result (List node) [NoPathExists]
  iter = \availableNodes, cameFrom, gScore, fScore ->
    if availableNodes |> Set.isEmpty then
      Err NoPathExists
    else
      lowestFScore =
        lowest =
          availableNodes
          |> Set.toList
          |> List.keepOks \node ->
              Dict.get fScore node
          |> List.min
        when lowest is
          Ok l -> l
          Err _ ->
            crash "there will be a lowest"

      current =
        tentativeCurrent =
          availableNodes
          |> Set.toList
          |> List.findFirst \node ->
              Dict.get fScore node == Ok lowestFScore

        when tentativeCurrent is
          Ok c -> c
          Err _ ->
            crash "We know visited nodes isn't empty, therefore there must be some node with the lowest fscore"

      if current == end then
        Ok (reconstructPath cameFrom current)
      else
        availableNodes2 = availableNodes |> Set.remove current

        (newAvailableNodes, newCameFrom, newGScore, newFScore) =
          neighbors current
          |> List.joinMap \neighbor ->
              Dict.get gScore current
              |> Result.map \currentGScore ->
                  tentativeGScore = currentGScore + weight current neighbor

                  neighborGScore =
                    Dict.get gScore neighbor
                    |> Result.withDefault (Num.toNat Num.maxU32)

                  if tentativeGScore < neighborGScore then
                    [(neighbor, tentativeGScore)]
                  else
                    []
              |> Result.withDefault []
          |> List.walk (availableNodes2, cameFrom, gScore, fScore)
              \(a, c, g, f), (neighbor, tentativeGScore) ->
                # if neighbor not in openSet ?
                ( a |> Set.remove current |> Set.insert neighbor
                , c |> Dict.insert neighbor current
                , g |> Dict.insert neighbor tentativeGScore
                , f |> Dict.insert neighbor (tentativeGScore + heuristic neighbor)
                )
        iter newAvailableNodes newCameFrom newGScore newFScore

  iter (Set.single start) (Dict.empty {}) (Dict.single start 0) (Dict.single start (heuristic start))

reconstructPath : Dict node node, node -> List node
reconstructPath = \cameFrom, current ->
  when cameFrom |> Dict.get current is
    Ok previous ->
      reconstructPath cameFrom previous
      |> List.append current
    Err _ ->
      [current]
