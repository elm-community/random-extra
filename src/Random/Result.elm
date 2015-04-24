module Random.Result where

import Random exposing (Generator)
import Random.Extra exposing (map)


result : Generator value -> Generator (Result error value)
result generator =
  map Ok generator


error : Generator error -> Generator (Result error value)
error generator =
  map Err generator
