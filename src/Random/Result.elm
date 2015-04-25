module Random.Result where

import Random       exposing (Generator, customGenerator, generate,float)
import Random.Extra exposing (map, frequency)


ok : Generator value -> Generator (Result error value)
ok generator =
  map Ok generator


error : Generator error -> Generator (Result error value)
error generator =
  map Err generator


result : Generator error -> Generator value -> Generator (Result error value)
result errorGenerator okGenerator =
  frequency
    [ (1, map Err errorGenerator)
    , (1, map Ok okGenerator)
    ] (map Ok okGenerator)
