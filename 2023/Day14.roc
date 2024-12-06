# Note: The roc compiler does NOT like this question
interface Day14.Solution
    exposes
      [ parse
      , input, test
      , part1, part2
      , part1TestAns, part2TestAns
      , part1Ans, part2Ans
      ]
    imports
      [ "input.txt" as input : Str
      , "test.txt" as test : Str
      , Platform.{ Platform }
      ]

part1TestAns = 136
part1Ans = 108792
part2TestAns = 64
part2Ans = 99118

parse : Str -> Platform
parse = Platform.fromStr

part1 : Platform -> Nat
part1 = \platform ->
  platform
    |> Platform.moveRocks North
    |> Platform.load

# Solution for part 2 causes a memory corruption bug in the compiler
# so I've set this up to debug the output, where I can find a loop,
# analyse where in the loop we'll be at the end of 1000000000 cycles,
# and the use that as our answer.
# Since it will recurse forever though I won't keep the unit tests.
tiltCycle : Platform, Nat -> Nat
tiltCycle = \platform, cycles ->
  dbg cycles
  afterCycle =
    platform
      |> Platform.moveRocks North
      |> Platform.moveRocks West
      |> Platform.moveRocks South
      |> Platform.moveRocks East
  dbg afterCycle |> Platform.load
  tiltCycle
    afterCycle
    (cycles - 1)

part2 : Platform -> Nat
part2 = \platform ->
  tiltCycle platform 1_000_000_000
