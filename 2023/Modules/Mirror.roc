interface Mirror
  exposes [Mirror, score, location]
  imports []

Mirror : [Vertical Nat, Horizontal Nat]

score : Mirror -> Nat
score = \mirror ->
  when mirror is
    Horizontal a -> 100 * a
    Vertical a -> a

location : Mirror -> Nat
location = \mirror ->
  when mirror is
    Horizontal a -> a
    Vertical a -> a
