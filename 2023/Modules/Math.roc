interface Math
  exposes [gcd, lcm, mod, magnitude, taxiCab]
  imports []

mod : Int a, Int a -> Int a
mod = \a, b ->
  rem = a % b
  if rem < 0 then
    b + rem
  else
    rem

taxiCab : (Int a, Int a), (Int a, Int a) -> Int a
taxiCab = \(r1, c1), (r2, c2) ->
  Num.abs (r1 - r2) + Num.abs (c1 - c2)

magnitude : (Num a, Num a) -> Num a
magnitude = \(r, c) ->
  Num.abs r + Num.abs c

gcd : Int a, Int a -> Int a
gcd = \n, m ->
  when (n, m) is
    (0, _) -> m
    (_, 0) -> n
    _ ->
      min = Num.min n m
      max = Num.max n m
      gcd min (max % min)

lcm : Int a, Int a -> Int a
lcm = \n, m ->
  when (n, m) is
    (0, 0) -> 0
    _ -> n * m // gcd n m
