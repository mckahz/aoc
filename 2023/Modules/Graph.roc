interface Graph
  exposes [ Graph, mincutN ]
  imports [Random]

Edge v : (v, v)
Contracted v : List v
Graph v :
  { verts : List v
  , edges : List (Edge v)
  }
MincutSolution v : { edges: Nat, partitions: List (List v) }

# Random

mincut : Graph v -> Random.Generator (Result (MincutSolution v) [NotEnoughVertices])
  where v implements Eq & Inspect
mincut = \graph ->
  verts = List.len graph.verts
  if verts < 2 then
    Random.constant (Err NotEnoughVertices)
  else
    graph
    |> makeContracted
    |> fullyContract
    |> Random.map mincutSolution
    |> Random.map Ok

mincutN : Graph v, Nat -> Random.Generator (Result (MincutSolution v) [NotEnoughVertices, MincutTooLarge])
  where v implements Eq & Inspect
mincutN = \graph, n ->
  verts = List.len graph.verts
  if verts < 2 then
    Random.constant (Err NotEnoughVertices)
  else
    findSolution (makeContracted graph) n

findSolution : Graph (Contracted v), Nat -> Random.Generator (Result (MincutSolution v) [MincutTooLarge])
  where v implements Eq & Inspect
findSolution = \graph, n ->
  verts = List.len graph.verts
  if verts > 6 then
    t = Num.ceiling (1 + Num.toF32 verts / Num.sqrt 2)

    solution <-
      graph
      |> contractUntil t
      |> Random.andThen \g -> findSolution g n
      |> Random.andThen

    when solution is
      Ok _ -> Random.constant solution
      _ ->
        graph
        |> contractUntil t
        |> Random.andThen \g ->
            findSolution g n
  else
    graph
    |> fullyContract
    |> Random.map mincutSolution
    |> Random.map \solution ->
        if solution.edges > n then
          Err MincutTooLarge
        else
          Ok solution

genEdge : Graph v -> Random.Generator (Edge v)
genEdge = \graph ->
  i <- Random.nat |> Random.andThen
  edgeIdx = i % List.len graph.edges
  when graph.edges |> List.get edgeIdx is
    Err _ -> crash ""
    Ok e -> Random.constant e

contractUntil : Graph (Contracted v), Nat -> Random.Generator (Graph (Contracted v))
  where v implements Eq & Inspect
contractUntil = \graph, minVerts ->
  verts = List.len graph.verts
  if verts <= minVerts then
    Random.constant graph
  else
    edge <- genEdge graph |> Random.andThen
    contractUntil (graph |> contract edge) minVerts

fullyContract : Graph (Contracted v) -> Random.Generator (Graph (Contracted v))
  where v implements Eq & Inspect
fullyContract = \graph ->
  contractUntil graph 2

# Deterministic

contract : Graph (Contracted v), Edge (Contracted v) -> Graph (Contracted v)
  where v implements Eq & Inspect
contract = \graph, (v1, v2) ->
  v = List.concat v1 v2

  verts =
    graph.verts
    |> List.dropIf \vert ->
        vert == v1 || vert == v2
    |> List.append v

  edges =
    graph.edges
    |> List.dropIf \(s, e) ->
        (s == v1 && e == v2)
        || (s == v2 && e == v1)
    |> List.map \(s, e) ->
        if s == v1 || s == v2 then
          (v, e)
        else if e == v1 || e == v2 then
          (s, v)
        else
          (s, e)

  { verts, edges }

makeContracted : Graph v -> Graph (Contracted v)
  where v implements Eq
makeContracted = \graph ->
  { verts: graph.verts |> List.map List.single
  , edges: graph.edges |> List.map \(s, e) -> ([s], [e])
  }

mincutSolution : Graph (Contracted v) -> { edges: Nat, partitions: List (List v) }
  where v implements Eq & Inspect
mincutSolution = \contractedGraph ->
  { edges: List.len contractedGraph.edges
  , partitions: contractedGraph.verts
  }
