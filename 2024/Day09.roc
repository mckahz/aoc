module [example, myInput, part1, part2]

import "inputs/day09-example.txt" as example : List U8
import "inputs/day09-input.txt" as myInput : List U8

Memory : List Cell

Cell : [Empty, File U64]

Chunk : {
    cell : Cell,
    size : U64,
}

parse : List U8 -> List Chunk
parse = \input ->
    input
    |> List.dropLast 1
    |> List.map List.single
    |> List.keepOks Str.fromUtf8
    |> List.keepOks Str.toU64
    |> List.walk { i: 0, chunks: [] } \state, n ->
        cell = if state.i % 2 == 0 then File (state.i // 2) else Empty
        chunk = {
            size: n,
            cell,
        }
        { i: state.i + 1, chunks: state.chunks |> List.append chunk }
    |> .chunks

getMemory : List Chunk -> Memory
getMemory = \chunks ->
    chunks
    |> List.map \chunk ->
        when chunk.cell is
            Empty -> Empty |> List.repeat chunk.size
            File id -> File id |> List.repeat chunk.size
    |> List.join

show : Memory -> Str
show = \memory ->
    memory
    |> List.map \cell ->
        when cell is
            Empty -> "."
            File id -> Num.toStr id
    |> List.prepend "\n"
    |> Str.joinWith ""

checksum : Memory -> U64
checksum = \memory ->
    memory
    |> List.mapWithIndex \cell, i ->
        when cell is
            File n -> n * i
            Empty -> 0
    |> List.sum

justFiles : Memory -> Memory
justFiles = \memory ->
    memory
    |> List.map \cell ->
        when cell is
            Empty -> []
            File id -> [File id]
    |> List.join

optimize : Memory -> Memory
optimize = \memory ->
    stoppingPoint = memory |> List.countIf \c -> c != Empty
    len = List.len memory

    optimizeHelp memory (justFiles memory |> List.reverse) 0 stoppingPoint
    |> List.takeFirst stoppingPoint
    |> List.concat (List.repeat Empty len)

optimizeHelp : Memory, Memory, U64, U64 -> Memory
optimizeHelp = \memory, revFiles, i, stoppingPoint ->
    cell = memory |> List.get i |> Result.withDefault Empty
    if i > stoppingPoint then
        memory
    else if cell == Empty then
        optimizeHelp
            (memory |> List.set i (revFiles |> List.first |> Result.withDefault Empty))
            (revFiles |> List.dropFirst 1)
            (i + 1)
            stoppingPoint
    else
        optimizeHelp
            memory
            revFiles
            (i + 1)
            stoppingPoint

part1 = \input ->
    memory = getMemory (parse input)
    optimize memory
    |> checksum

optimizeContiguous : List Chunk -> List Chunk
optimizeContiguous = \chunks ->
    maxId =
        chunks
        |> List.map \chunk ->
            when chunk.cell is
                Empty -> 0
                File id -> id
        |> List.max
        |> Result.withDefault 0
    optimizeContiguousHelp chunks maxId

optimizeContiguousHelp : List Chunk, U64 -> List Chunk
optimizeContiguousHelp = \chunks, idToMove ->
    if idToMove == 0 then
        chunks
    else
        chunkToMoveIndex =
            chunks
            |> List.findLastIndex \chunk ->
                when chunk.cell is
                    Empty -> Bool.false
                    File id -> id == idToMove
            |> Result.withDefault 0

        chunkToMove =
            chunks
            |> List.get chunkToMoveIndex
            |> Result.withDefault { cell: Empty, size: 0 }

        firstAvailableSpace =
            chunks
            |> List.findFirstIndex \chunk ->
                chunk.cell == Empty && chunk.size >= chunkToMove.size

        newChunks =
            when firstAvailableSpace is
                Err _ -> chunks
                Ok i if i > chunkToMoveIndex -> chunks
                Ok i ->
                    { before, others: after } =
                        chunks
                        |> List.set chunkToMoveIndex { chunkToMove & cell: Empty }
                        |> List.update i \c -> { cell: Empty, size: c.size - chunkToMove.size }
                        |> List.splitAt i
                    before
                    |> List.append chunkToMove
                    |> List.concat after

        optimizeContiguousHelp newChunks (idToMove - 1)

part2 = \input ->
    chunks = parse input
    chunks
    |> optimizeContiguous
    |> getMemory
    |> checksum
