interface Spring
  exposes [Spring, fromStr, toStr]
  imports []

Spring : [Operational, Damaged, Unknown]

fromStr : Str -> Result Spring [UnrecognizedSpring]
fromStr = \str ->
  when str is
    "#" -> Ok Damaged
    "." -> Ok Operational
    "?" -> Ok Unknown
    _ -> Err UnrecognizedSpring

toStr : Spring -> Str
toStr = \spring ->
  when spring is
    Damaged -> "#"
    Operational -> "."
    Unknown -> "?"

