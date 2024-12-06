app "dayN"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports
      [ pf.Task
      , parser.Core.{ Parser, keep, skip, oneOf, sepBy, const, many }
      , parser.String.{ string, parseStr, digits }
      , "test.txt" as test : Str
      , "input.txt" as input : Str
      ]
    provides [main] to pf

Input : 

expect
  ans = solution1 (parse test)
  ans == 0
#expect
#  ans = solution1 (parse input)
#  ans == 0
#expect
#  ans = solution2 (parse test)
#  ans == 0
#expect
#  ans = solution2 (parse input)
#  ans == 0

main = Task.ok {}

parse : Str -> Input
parse = \str -> str |> Str.split "\n"

solution1 : Input -> Nat
solution1 = \lines ->
  ans = 0
  dbg ans
  0

solution2 : Input -> Nat
solution2 = \lines ->
  ans = 0
  dbg ans
  0
