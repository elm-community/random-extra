module Random.Pcg.Order exposing (..)

{-| An extra random generator for the `Order` type.

@docs order
-}

import Random.Pcg exposing (Generator, map)
import Random.Pcg.Extra exposing (sample)


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
