
executeSc : Memory, List U64, U64, U64 -> Bool
executeSc = \memory, program, ip, matching ->
    op = program |> List.get ip |> Result.try fromU64
    operand = program |> List.get (ip + 1) |> Result.withDefault 0
    combo = \o ->
        when o is
            0 | 1 | 2 | 3 -> o
            4 -> memory.a
            5 -> memory.b
            6 -> memory.c
            _ -> crash "no"

    when op is
        Err _ -> matching == List.len program
        Ok Adv ->
            d = 2 |> Num.powInt (combo operand)
            executeSc { memory & a: memory.a // d } program (ip + 2) matching

        Ok Bxl ->
            executeSc { memory & b: memory.b |> Num.bitwiseXor operand } program (ip + 2) matching

        Ok Bst ->
            executeSc { memory & b: combo operand % 8 } program (ip + 2) matching

        Ok Jnz if memory.a == 0 -> executeSc memory program (ip + 2) matching
        Ok Jnz -> executeSc memory program operand matching
        Ok Bxc -> executeSc { memory & b: memory.b |> Num.bitwiseXor memory.c } program (ip + 2) matching
        Ok Out ->
            output = combo operand % 8
            if program |> List.dropFirst matching |> List.startsWith [output] then
                executeSc memory program (ip + 2) (matching + 1)
            else
                Bool.false

        Ok Bdv ->
            d = 2 |> Num.powInt (combo operand)
            executeSc { memory & b: memory.a // d } program (ip + 2) matching

        Ok Cdv ->
            d = 2 |> Num.powInt (combo operand)
            executeSc { memory & c: memory.a // d } program (ip + 2) matching

part2Old = \input ->
    (memory, program) = parse input
    findSolution : U64 -> U64
    findSolution = \a ->
        dbg a
        if executeSc { memory & a } program 0 0 then
            a
        else
            findSolution (a + 1)
    findSolution 184571811
