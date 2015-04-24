module Random.Float where

import Random exposing (Generator, float, maxInt, minInt)


{-| Generator that generates any float
-}
anyFloat : Generator Float
anyFloat = float (toFloat minInt) (toFloat maxInt)

{-| Generator that generates any positive float
-}
positiveFloat : Generator Float
positiveFloat = float 0 (toFloat maxInt)

{-| Generator that generates any negative float
-}
negativeFloat : Generator Float
negativeFloat = float (toFloat minInt) 0

{-| Generator that generates a float greater than a given float
-}
floatGreaterThan : Float -> Generator Float
floatGreaterThan value = float value (toFloat maxInt)

{-| Generator that generates a float less than a given float
-}
floatLessThan : Float -> Generator Float
floatLessThan value = float (toFloat minInt) value

{-| Generator that generates a float between 0 and 1
-}
probability : Generator Float
probability = float 0 1

{-| Generator that generates a float between -1 and 0
-}
negativeProbability : Generator Float
negativeProbability = float -1 0

{-| Generator that generates a float between - 1 and 1
-}
absoluteProbability : Generator Float
absoluteProbability = float -1 1

{-}
normal : Float -> Float -> Float -> Generator Float
normal start end standardDeviation =
  let normalDistribution mean stdDev x =
        if stdDev == 0 then x
        else
          let scale = 1 / (stdDev * sqrt (2 * pi))
              exponent = ((x - mean) * (x - mean)) / (2 * stdDev * stdDev)
          in
            scale * (e ^ -exponent)

  in
    map (normalDistribution ((end - start) / 2) standardDeviation) (float start end)

standardNormal : Generator Float
standardNormal = normal (toFloat minInt + 1) (toFloat maxInt) 1

gaussian : Float -> Float -> Float -> Generator Float
gaussian = normal

-}
