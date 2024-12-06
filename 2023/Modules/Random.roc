interface Random
  exposes
    [ Generator, Seed
    , i32, nat, f32, uniform, weighted, constant
    , list, pair
    , map, map2, map3
    , unique2
    , andThen, lazy
    , generate
    , generateUntil
    , step, mkSeed, independentSeed
    ]
  imports []

# PRIMITIVE GENERATORS

nat : Generator Nat
nat =
  i32
  |> map \n ->
      n
      |> Num.abs
      |> Num.toNat

i32 : Generator I32
i32 = @Generator \seed0 ->
  ( lo, hi ) = (Num.minI32 // 2 + 1, Num.maxI32 // 2)
  range = hi - lo + 1

  if (Num.bitwiseAnd (range - 1) range) == 0 then
    ( (Num.shiftRightZfBy (Num.bitwiseAnd (peel seed0) (range - 1)) 0) + lo, next seed0 )
  else
    threshhold =
      Num.shiftRightZfBy (Num.rem (Num.shiftRightZfBy (0-range) 0) range) 0

    accountForBias : Seed -> ( I32, Seed )
    accountForBias = \seed ->
      x = peel seed
      seedN = next seed
      if x < threshhold then
        # in practice this recurses almost never
        accountForBias seedN
      else
        ( Num.rem x range + lo, seedN )

    accountForBias seed0

generateUntil : Generator a, (a -> Bool) -> Generator a
generateUntil = \gen, p ->
  v <- gen |> andThen
  if p v then
    constant v
  else
    generateUntil gen p

f32 : F32, F32 -> Generator F32
f32 = \a, b ->
  @Generator \seed0 ->
    seed1 = next seed0
    n0 = peel seed0
    n1 = peel seed1
    hi = Num.toF32 (Num.bitwiseAnd 0x03FFFFFF n0) * 1.0
    lo = Num.toF32 (Num.bitwiseAnd 0x07FFFFFF n1) * 1.0
    val = ((hi * 134217728.0) + lo) / 9007199254740992.0

    range = Num.abs (b - a)
    scaled = val * range + a
    ( scaled, next seed1 )

constant : a -> Generator a
constant = \value -> @Generator \seed -> (value, seed)

# DATA STRUCTURES

pair : Generator a, Generator b -> Generator (a, b)
pair = \genA, genB ->
  map2 genA genB \a, b -> (a, b)

list : Generator a, Nat -> Generator (List a)
list = \(@Generator gen), n ->
  @Generator \seed ->
    listHelp [] n gen seed

listHelp : List a, Nat, (Seed -> (a, Seed)), Seed -> (List a, Seed)
listHelp = \l, n, gen, seed ->
  if n < 1 then
    (l, seed)
  else
    (value, newSeed) = gen seed
    listHelp (l |> List.append value) (n-1) gen newSeed

# ENUMERATIONS

uniform : a, List a -> Generator a
uniform = \value, valueList ->
  weighted (addOne value) (valueList |> List.map addOne)

addOne : a -> (F32, a)
addOne = \value -> ( 1, value )

weighted : (F32, a), List (F32, a) -> Generator a
weighted = \first, others ->
  normalize = \(weight, _) ->
    Num.abs weight

  total =
    normalize first + List.sum (others |> List.map normalize)

  f32 0 total
  |> map \countdown ->
      getByWeight first others countdown

getByWeight : (F32, a), List (F32, a), F32 -> a
getByWeight = \(weight, value), others, countdown ->
  when others is
    [] -> value
    [second, .. as otherOthers] ->
      if countdown <= Num.abs weight then
        value
      else
        getByWeight second otherOthers (countdown - Num.abs weight)

# CUSTOM GENERATORS

map : Generator a, (a -> b) -> Generator b
map = \@Generator genA, func ->
  @Generator \seed0 ->
    (a, seed1) = genA seed0
    (func a, seed1)

map2 : Generator a, Generator b, (a, b -> c) -> Generator c
map2 = \@Generator genA, @Generator genB, func ->
  @Generator \seed0 ->
    (a, seed1) = genA seed0
    (b, seed2) = genB seed1
    (func a b, seed2)

map3 : Generator a, Generator b, Generator c, (a, b, c -> d) -> Generator d
map3 = \@Generator genA, @Generator genB, @Generator genC, func ->
  @Generator \seed0 ->
    (a, seed1) = genA seed0
    (b, seed2) = genB seed1
    (c, seed3) = genC seed2
    (func a b c, seed3)

andThen : Generator a, (a -> Generator b) -> Generator b
andThen = \@Generator genA, callback ->
  @Generator \seed ->
    (result, newSeed) = genA seed
    @Generator genB = callback result
    genB newSeed

lazy : ({} -> Generator a) -> Generator a
lazy = \callback ->
  @Generator \seed ->
    @Generator gen = callback {}
    gen seed

# IMPLEMENTATION

Seed := (I32, I32)

# step the RNG to produce the next seed
# this is incredibly simple: multiply the state by a constant factor, modulus it
# by 2^32, and add a magic addend. The addend can be varied to produce independent
# RNGs, so it is stored as part of the seed. It is given to the new seed unchanged.
next : Seed -> Seed
next = \@Seed (state0, incr) ->
  # The magic constant is from Numerical Recipes and is inlined for perf.
  @Seed
    ( state0
      |> Num.mulWrap 1664525
      |> Num.addWrap incr
      |> Num.shiftRightZfBy 0
    , incr
    )

# obtain a psuedorandom 32-bit integer from a seed
peel : Seed -> I32
peel = \@Seed (state, _) ->
  # This is the RXS-M-SH version of PCG, see section 6.3.4 of the paper
  # and line 184 of pcg_variants.h in the 0.94 (non-minimal) C implementation,
  # the latter of which is the source of the magic constant.
  word =
    state
    |> Num.shiftRightZfBy 28
    |> Num.addWrap 4
    |> Num.shiftRightZfBy (Num.toU8 state)
    |> Num.bitwiseXor state
    |> Num.mulWrap 277803737

  Num.shiftRightZfBy (Num.bitwiseXor (Num.shiftRightZfBy word 22) word) 0

Generator a := Seed -> (a, Seed)

step : Generator a, Seed -> (a, Seed)
step = \@Generator generator, seed ->
  generator seed

mkSeed : I32 -> Seed
mkSeed = \x ->
  # the default increment magic constant is taken from Numerical Recipes
  @Seed (state1, incr) =
    next (@Seed (0, 1013904223))

  state2 = Num.shiftRightZfBy (state1 |> Num.addWrap x) 0

  next (@Seed (state2, incr))

independentSeed : Generator Seed
independentSeed =
  @Generator \seed0 ->
    makeIndependentSeed = \state, b, c ->
      next (@Seed (state, Num.shiftRightZfBy (Num.bitwiseOr 1 (Num.bitwiseXor b c)) 0))

    step (map3 i32 i32 i32 makeIndependentSeed) seed0

generate : Generator a, Seed -> a
generate = \@Generator gen, seed ->
  gen seed |> .0

unique2 : Generator a -> Generator (a, a) where a implements Eq & Inspect
unique2 = \gen ->
  v1 <- gen |> andThen
  v2 <- gen |> andThen
  if v1 == v2 then
    unique2 gen
  else
    constant (v1, v2)
