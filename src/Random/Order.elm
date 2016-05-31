module Random.Order exposing (..)

{-| An extra random generator for the `Order` type.

@docs order
-}

import Random exposing (Generator, map)
import Random.Extra exposing (sample)


{-| Generate a random order with equal probability.
-}
order : Generator Order
order =
    sample
        [ LT
        , EQ
        , GT
        ]
        |> map (Maybe.withDefault EQ)
