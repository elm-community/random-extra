module Random.Order exposing (..)

{-| List of Order Generators

# Generators
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
