module Random.Bool where

import Random exposing (Generator, generate, customGenerator, int)

{-| Random Bool generator
-}
bool : Generator Bool
bool =
  customGenerator
    (\seed ->
        let (value1, seed1) = generate (int 0 1) seed
            (value2, seed2) = generate (int 0 1) seed1
        in
          if (value1 + value2) == 1
          then
            (False, seed2)
          else
            (True, seed2))
