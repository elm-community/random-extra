module Random.Maybe where
{-| List of Maybe Generators

# Generators
@docs maybe
-}

import Random       exposing (Generator)
import Random.Extra exposing (constant, map, frequency)

{-| Generate a Maybe from a generator. Will generate Nothings 50% of the time.
-}
maybe : Generator a -> Generator (Maybe a)
maybe generator =
  frequency
    [ (1, constant Nothing)
    , (1, map Just generator)
    ] (constant Nothing)
