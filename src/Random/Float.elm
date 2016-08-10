module Random.Float exposing (..)

{-| Extra randomized functions on floats.

# Arithmetic Generators
@docs anyFloat, positiveFloat, negativeFloat, floatGreaterThan, floatLessThan

# Gaussian Generators
@docs normal, standardNormal

-}

import Random exposing (Generator, map, float, maxInt, minInt)


{-| A generator that generates any float
-}
anyFloat : Generator Float
anyFloat =
    float (toFloat minInt) (toFloat maxInt)


{-| A generator that generates any positive float
-}
positiveFloat : Generator Float
positiveFloat =
    float 0 (toFloat maxInt)


{-| A generator that generates any negative float
-}
negativeFloat : Generator Float
negativeFloat =
    float (toFloat minInt) 0


{-| A generator that generates a float greater than a given float
-}
floatGreaterThan : Float -> Generator Float
floatGreaterThan value =
    float value (toFloat maxInt)


{-| A generator that generates a float less than a given float
-}
floatLessThan : Float -> Generator Float
floatLessThan value =
    float (toFloat minInt) value


{-| Create a generator of floats that is normally distributed with
given mean and standard deviation.
-}
normal : Float -> Float -> Generator Float
normal mean stdDev =
    map (\u -> u * stdDev + mean) standardNormal


{-| A generator that follows a standard normal distribution (as opposed to
a uniform distribution)
-}
standardNormal : Generator Float
standardNormal =
    Random.map2
        (\u theta -> sqrt (-2 * logBase e (1 - max 0 u)) * cos theta)
        (float 0 1)
        (float 0 (2 * pi))
