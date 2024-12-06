interface Cache
  exposes [Cache, map, updateCache, andThen, store, ignore, get, run, sequence]
  imports []

Cache a cache := (cache -> (a, cache))

map : Cache a cache, (a -> b) -> Cache b cache
map = \@Cache c, f -> @Cache \cache ->
  (val, newCache) = c cache
  (f val, newCache)

updateCache : Cache r cache, (cache -> cache) -> Cache r cache
updateCache = \@Cache c, f -> @Cache \cache ->
  (val, newCache) = c cache
  (val, f newCache)

andThen : Cache a cache, (a -> Cache b cache) -> Cache b cache
andThen = \@Cache cache, chainCache -> @Cache \cached ->
  (v, newCached) = cache cached
  @Cache newCache = (chainCache v)
  newCache newCached

sequence : List ({} -> Cache a cache) -> Cache (List a) cache
sequence = \cacheThunks -> @Cache \cached ->
  cacheThunks
    |> List.walk ([], cached) \(results, newCache), cacheThunk ->
        @Cache cache = cacheThunk {}
        (result, newerCache) = cache newCache
        (results |> List.append result, newerCache)

store : cache -> Cache {} cache
store = \cache -> @Cache \_ -> ({}, cache)

ignore : a -> Cache a cache
ignore = \v -> @Cache \cache -> (v, cache)

get : Cache cache cache
get = @Cache \cache -> (cache, cache)

run : Cache r cache, cache -> r
run = \@Cache cached, c -> cached c |> .0
