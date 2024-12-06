interface Path
  exposes [astar, dijkstra, longest, infinity]
  imports []


infinity : Nat
infinity = Num.toNat Num.maxU32

reconstructPath : node, Dict node node -> List node
  where node implements Hash & Eq & Inspect
reconstructPath = \current, cameFrom ->
  when cameFrom |> Dict.get current is
    Ok previous ->
      reconstructPath previous cameFrom
      |> List.append current
    Err _ ->
      [current]

dijkstra : node, node, (node -> List node), (node, node -> Nat) -> Result (List node) [NoPathExists]
  where node implements Hash & Eq & Inspect
dijkstra = \start, end, next, weight ->
  iter : { cameFrom: Dict node node, distances: Dict node Nat, available: List node } ->
    Result (List node) [NoPathExists]
  iter = \state ->
    current =
      state.available
      |> List.last
      |> Result.withDefault end

    if current == end then
      Ok (reconstructPath end state.cameFrom)
    else if state.available |> List.isEmpty then
      Err NoPathExists
    else
      List.walk (next current) { state & available : state.available |> List.dropLast 1 } \oldState, neighbour ->
        distance =
          when Dict.get oldState.distances current is
            Ok dist -> dist + weight current neighbour
            Err _ -> crash "we always insert the current nodes distance before we use it"

        dbg oldState.available |> List.len

        updateState =
          when (oldState.distances |> Dict.get neighbour) is
          Err _ -> Bool.true
          Ok p if distance < p -> Bool.true
          Ok _ -> Bool.false

        if updateState then
          { cameFrom: Dict.insert oldState.cameFrom neighbour current
          , distances: Dict.insert oldState.distances neighbour distance
          , available:
              nextShortest =
                oldState.available
                |> List.findLastIndex \node ->
                    nodeDist =
                      Dict.get oldState.distances node
                      |> Result.withDefault (Num.toNat Num.maxU32)
                    nodeDist > distance
                |> Result.map \m -> m + 1
                |> Result.withDefault 0

              { before, others } =
                oldState.available
                |> List.dropIf \node -> node == neighbour
                |> List.split nextShortest

              before |> List.append neighbour |> List.concat others
          }
        else
          oldState
      |> iter

  iter
    { cameFrom: Dict.empty {}
    , distances: Dict.single start 0
    , available: [start]
    }


astar : node, node, (node -> List node), (node, node -> Nat), (node -> Nat) -> Result (List node) [NoPathExists]
  where node implements Hash & Eq & Inspect
astar = \start, end, next, weight, heuristic ->
  iter : { cameFrom: Dict node node, distances: Dict node Nat, available: Set node, estimates: Dict node Nat } ->
    Result (List node) [NoPathExists]
  iter = \state ->
    minEstimate =
      state.estimates
      |> Dict.toList
      |> List.keepOks \(node, dist) ->
          if state.available |> Set.contains node then
            Ok dist
          else
            Err {}
      |> List.min
      |> Result.withDefault 0

    current =
      state.estimates
      |> Dict.toList
      |> List.findFirst \(node, dist) ->
          dist == minEstimate
          && (state.available |> Set.contains node)
      |> Result.map .0
      |> Result.withDefault start

    dbg minEstimate

    minDist =
      state.distances
      |> Dict.get current
      |> Result.withDefault (Num.toNat Num.maxU32)

    if current == end then
      Ok (reconstructPath end state.cameFrom)
    else if state.available |> Set.isEmpty then
      Err NoPathExists
    else
      neighbours = next current
      newState =
        List.walk neighbours state \oldState, neighbour ->
          previousDistance =
            oldState.distances
            |> Dict.get neighbour
            |> Result.withDefault (Num.toNat Num.maxU32)

          distance = minDist + weight current neighbour

          if distance < previousDistance then
            { cameFrom: Dict.insert oldState.cameFrom neighbour current
            , distances: Dict.insert oldState.distances neighbour distance
            , estimates: Dict.insert oldState.estimates neighbour (distance + heuristic neighbour)
            , available: Set.insert oldState.available neighbour
            }
          else
            oldState
      iter { newState & available: newState.available |> Set.remove current }

  iter
    { cameFrom: Dict.empty {}
    , distances: Dict.single start 0
    , estimates: Dict.single start (heuristic start)
    , available: Set.single start
    }

weighPath : List node, (node, node -> Nat) -> Nat
  where node implements Hash & Eq & Inspect
weighPath = \path, weight ->
  List.map2 path (path |> List.dropFirst 1) weight
  |> List.sum

longest : node, node, (node -> List node), (node, node -> Nat) -> Result (List node) [NoPathExists]
  where node implements Hash & Eq & Inspect
longest = \start, end, next, weight ->
  iter : { current: node, cameFrom: Dict node node } -> Result (List node) [NoPathExists]
  iter = \state ->
    path = reconstructPath state.current state.cameFrom

    neighbours =
      next state.current
      |> List.dropIf \neighbour ->
          path |> List.contains neighbour

    if state.current == end then
      Ok path

    else if neighbours |> List.isEmpty then
      Err NoPathExists

    else
      List.walk neighbours (Err NoPathExists) \longestPath, neighbour ->
        contender =
          iter
            { current: neighbour
            , cameFrom: state.cameFrom |> Dict.insert neighbour state.current
            }

        when (longestPath, contender) is
          (Ok l, Ok c) if weighPath l weight > weighPath c weight -> longestPath
          (_, Err _) -> longestPath
          _ -> contender

  iter
    { current: start
    , cameFrom: Dict.empty {}
    }

