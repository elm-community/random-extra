module Random.Bool exposing (weightedBool)

{-| Extra functions for generating Bools.


# Values

@docs weightedBool

-}

import Random exposing (Generator)


{-| Generates True with probability given by the argument (number 0..1, clamped
to these bounds if needed)

    weightedBool -0.5 -- always generates False

    weightedBool 0 -- always generates False

    weightedBool 0.25 -- biased coin, generates True 25% of the time

    weightedBool 0.5 -- fair coin, same as Random.Extra.bool

    weightedBool 0.75 -- biased coin, generates True 75% of the time

    weightedBool 1 -- always generates True

    weightedBool 1.5 -- always generates True

-}
weightedBool : Float -> Generator Bool
weightedBool probability =
    let
        clampedProbability : Float
        clampedProbability =
            clamp 0 1 probability
    in
    Random.float 0 1
        |> Random.map (\float -> float <= clampedProbability)
