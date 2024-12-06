interface Util
  exposes [unique]
  imports []

unique : List a -> List a where a implements Hash & Eq
unique = \xs -> xs |> Set.fromList |> Set.toList
