interface Boundary
  exposes [Boundary, fromDigPlan, countDugOut]
  imports [ DigPlan.{ DigPlan, Step } ]

Coord : (Nat, Nat)
Length : Nat
Boundary :
  { corners : List Corner
  , edges : List Edge
  , width : Nat
  , height : Nat
  }
Corner :
  { position : Coord
  , direction : [BottomLeft, BottomRight, TopLeft, TopRight]
  }
VerticalEdge :
  { len: Length
  , col: Nat
  , top: { row: Nat, direction: [TopLeft, TopRight] }
  , bottom: { row: Nat, direction: [BottomLeft, BottomRight] }
  , direction: [Down, Up]
  }
HorizontalEdge :
  { len: Length
  , row: Nat
  , left: { col: Nat, direction: [BottomLeft, TopLeft] }
  , right: { col: Nat, direction: [BottomRight, TopRight] }
  , direction: [Right, Left]
  }
Edge :
  [ Vertical VerticalEdge
  , Horizontal HorizontalEdge
  ]

isAbove : Coord, Coord -> Bool
isAbove = \(r1, _), (r2, _) -> r1 < r2

isBelow : Coord, Coord -> Bool
isBelow = \(r1, _), (r2, _) -> r1 > r2

isRightOf : Coord, Coord -> Bool
isRightOf = \(_, c1), (_, c2) -> c1 > c2

isLeftOf : Coord, Coord -> Bool
isLeftOf = \(_, c1), (_, c2) -> c1 < c2

getCorners : DigPlan -> List Corner
getCorners = \digPlan ->
  unnormalizedPositions =
    digPlan
    |> List.walk [(0, 0)] \path, { amount, direction } ->
        (row, col) = path |> List.last |> Result.withDefault (0, 0)

        newPos =
          when direction is
            Right -> (row, col + amount)
            Left -> (row, col - amount)
            Up -> (row - amount, col)
            Down -> (row + amount, col)

        path |> List.append newPos
    |> List.dropLast 1

  minRow =
    unnormalizedPositions
    |> List.map .0
    |> List.min
    |> Result.withDefault 0

  minCol =
    unnormalizedPositions
    |> List.map .1
    |> List.min
    |> Result.withDefault 0

  positions =
    unnormalizedPositions
    |> List.map \(ur, uc) -> (Num.toNat (ur - minRow), Num.toNat (uc - minCol))

  rotatedForwards =
    List.concat
      (positions |> List.takeLast 1)
      (positions |> List.dropLast 1)

  rotatedBackwards =
    List.concat
      (positions |> List.dropFirst 1)
      (positions |> List.takeFirst 1)

  List.map3 rotatedForwards positions rotatedBackwards
    \previous, position, next ->
      direction =
        when {} is
          _ if (position |> isAbove previous) && (position |> isRightOf next) -> TopRight
          _ if (position |> isBelow previous) && (position |> isRightOf next) -> BottomRight
          _ if (position |> isAbove previous) && (position |> isLeftOf next) -> TopLeft
          _ if (position |> isBelow previous) && (position |> isLeftOf next) -> BottomLeft

          _ if (position |> isRightOf previous) && (position |> isAbove next) -> TopRight
          _ if (position |> isRightOf previous) && (position |> isBelow next) -> BottomRight
          _ if (position |> isLeftOf previous) && (position |> isAbove next) -> TopLeft
          _ if (position |> isLeftOf previous) && (position |> isBelow next) -> BottomLeft

          _ -> crash "invalid directions"

      { position, direction }

fromDigPlan : DigPlan -> Boundary
fromDigPlan = \digPlan ->
  corners = getCorners digPlan
    |> List.map \corner ->
        corner

  width =
    corners
    |> List.map .position
    |> List.map .1
    |> List.max
    |> Result.withDefault 0
    |> Num.add 1

  height =
    corners
    |> List.map .position
    |> List.map .0
    |> List.max
    |> Result.withDefault 0
    |> Num.add 1

  edges =
    rotated =
      List.concat
        (corners |> List.dropFirst 1)
        (corners |> List.takeFirst 1)

    List.map2 corners rotated \{ position: p1, direction: d1 }, { position: p2, direction: d2 } ->
      if (p1 |> isAbove p2) || (p1 |> isBelow p2) then
        top =
          { row: Num.min p1.0 p2.0
          , direction:
              when (d1, d2) is
                (TopLeft, _) | (_, TopLeft) -> TopLeft
                (TopRight, _) | (_, TopRight) -> TopRight
                _ -> crash "at least one should be a top!"
          }

        bottom =
          { row: Num.max p1.0 p2.0
          , direction:
              when (d1, d2) is
                (BottomLeft, _) | (_, BottomLeft) -> BottomLeft
                (BottomRight, _) | (_, BottomRight) -> BottomRight
                _ -> crash "at least one should be a bottom!"
          }

        direction =
          if p1 |> isAbove p2 then Down else Up

        Vertical
          { len: 1 + bottom.row - top.row
          , col: p1.1
          , top
          , bottom
          , direction
          }

      else if (p1 |> isLeftOf p2) || (p1 |> isRightOf p2) then
        left =
          { col: Num.min p1.1 p2.1
          , direction:
              when (d1, d2) is
                (TopLeft, _) | (_, TopLeft) -> TopLeft
                (BottomLeft, _) | (_, BottomLeft) -> BottomLeft
                _ -> crash "at least one should be a top!"
          }

        right =
          { col: Num.max p1.1 p2.1
          , direction:
              when (d1, d2) is
                (TopRight, _) | (_, TopRight) -> TopRight
                (BottomRight, _) | (_, BottomRight) -> BottomRight
                _ -> crash "at least one should be a bottom!"
          }

        direction =
          if p1 |> isLeftOf p2 then Right else Left

        Horizontal
          { len: 1 + right.col - left.col
          , row: p1.0
          , left
          , right
          , direction
          }

      else
        crash "no corners like this should exist"

  { corners, edges, width, height }

verticals : Boundary -> List VerticalEdge
verticals = \boundary ->
  boundary.edges
  |> List.joinMap \edge ->
      when edge is
        Vertical x -> [x]
        Horizontal _ -> []

areaRightOf : VerticalEdge, List VerticalEdge -> Nat
areaRightOf = \edge, verticalEdges ->
  range =
    { top:
        when edge.top.direction is
          TopLeft -> edge.top.row + 1
          TopRight -> edge.top.row
    , bottom:
        when edge.bottom.direction is
          BottomLeft -> edge.bottom.row - 1
          BottomRight -> edge.bottom.row
    }

  edgesToRight : List VerticalEdge
  edgesToRight =
    verticalEdges
    |> List.keepIf \{ col, top, bottom } ->
        edge.col < col
        && top.row <= range.bottom
        && range.top <= bottom.row

  closestCol : Nat
  closestCol =
    result =
      edgesToRight
      |> List.map .col
      |> List.min

    when result is
      Ok r -> r
      Err _ ->
        crash "There should be at least one edge to the right"

  closestEdges : List VerticalEdge
  closestEdges =
    edgesToRight
    |> List.keepIf \{ col } -> col == closestCol

  unobstructedArea : Nat
  unobstructedArea =
    (closestCol - edge.col - 1) * (1 + range.bottom - range.top)

  simulatedRanges : List { top: Nat, bottom: Nat }
  simulatedRanges =
    List.walk closestEdges [range] \ranges, closestEdge ->
      ranges
      |> List.joinMap \r ->
          List.join
            [ if r.top < closestEdge.top.row then
                [ { top: r.top
                  , bottom: closestEdge.top.row - 1
                  }
                ] else []
            , if closestEdge.bottom.row < r.bottom then
                [ { top: closestEdge.bottom.row + 1
                  , bottom: r.bottom
                  }
                ] else []
            ]

  List.walk simulatedRanges unobstructedArea \area, simulatedRange ->
    areaRightOf
      { col: closestCol - 1
      , len: 1 + simulatedRange.bottom - simulatedRange.top
      , top: { row: simulatedRange.top, direction: TopRight }
      , bottom: { row: simulatedRange.bottom, direction: BottomRight }
      , direction: edge.direction
      } verticalEdges
      + area

leftEdges : Boundary -> List VerticalEdge
leftEdges = \boundary ->
  # this function is janky as shit, but it'll do
  verticalEdges = verticals boundary

  includeFirst = \cond ->
    verticalEdges
    |> List.walk ([], None, Bool.false) \(edges, direction, lastWasIncluded), edge ->
        when direction is
            None if cond -> ([edge], Just edge.direction, Bool.true)
            None -> ([], Just edge.direction, Bool.false)
            Just dir ->
              if (dir == edge.direction) == !lastWasIncluded then
                (edges, Just edge.direction, Bool.false)
              else
                (edges |> List.append edge, Just edge.direction, Bool.true)
    |> .0

  assumeFirstIsLeft = includeFirst Bool.true
  if assumeFirstIsLeft |> List.any \{ col } -> col + 1 == boundary.width then
    includeFirst Bool.false
  else
    assumeFirstIsLeft

countDugOut : Boundary -> Nat
countDugOut = \boundary ->
  outside : Nat
  outside =
    boundary.edges
    |> List.map \edge ->
        when edge is
          Vertical { len } -> len
          Horizontal { len } -> len
    |> List.sum
    |> Num.sub (List.len boundary.corners)

  verticalEdges : List VerticalEdge
  verticalEdges =
    verticals boundary

  inside : Nat
  inside =
    leftEdges boundary
    |> List.map \edge -> areaRightOf edge verticalEdges
    |> List.sum

  outside + inside

contains : Boundary, Coord -> Bool
contains = \boundary, (row, col) ->
  boundary.edges
  |> List.any \edge ->
      when edge is
        Vertical vedge -> vedge.top.row <= row && row <= vedge.bottom.row && col == vedge.col
        Horizontal hedge -> hedge.left.col <= col && col <= hedge.right.col && row == hedge.row

toStr : Boundary -> Str
toStr = \boundary ->
  graphemes =
    i <- List.joinMap (List.range { start: At 0, end: Length boundary.height })
    j <- List.map (List.range { start: At 0, end: Length boundary.width })
    if boundary |> contains (i, j) then
      "#"
    else
      "."
  graphemes
  |> List.chunksOf boundary.width
  |> List.map \row -> row |> Str.joinWith ""
  |> List.prepend ""
  |> Str.joinWith "\n"
