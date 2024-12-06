interface Parsing
  exposes [alpha, alphas]
  imports
    [ parser.Core.{ Parser, keep, skip, oneOf, sepBy, const, many, map }
    , parser.String.{ string, parseStr }
    , Machine.{ Machine }
    , Module.{ Module, Name }
    ]


alpha : Parser (List U8) Str
alpha =
  "abcdefghijklmnopqrstuvwxyz"
  |> Str.graphemes
  |> List.map string
  |> oneOf

alphas : Parser (List U8) Str
alphas =
  many alpha
  |> map \strs -> strs |> Str.joinWith ""

