module Random.Result exposing (..)

{-| List of Result Generators

# Generators
@docs ok, error, result

-}

import Random exposing (Generator, generate, map, float)
import Random.Extra exposing (frequency)


{-| Generate an ok result from a random generator of values
-}
ok : Generator value -> Generator (Result error value)
ok generator =
    map Ok generator


{-| Generate an error result from a random generator of errors
-}
error : Generator error -> Generator (Result error value)
error generator =
    map Err generator


{-| Generate an `Ok` result or an `Err` result with 50-50 chance.

If you want to generate results with a different frequency, use `frequency`.
-}
result : Generator error -> Generator value -> Generator (Result error value)
result errorGenerator okGenerator =
    choices
        [ error errorGenerator
        , ok okGenerator
        ]
