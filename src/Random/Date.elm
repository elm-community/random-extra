module Random.Date exposing (weekday, month)

{-| Extra randomized functions on dates (see `elm/time`).

We only provide generators for days and months. Other generators are trivial for you to implement yourself and can be made specific to your needs.


# Generators

@docs weekday, month

-}

import Random exposing (Generator, map)
import Random.Extra exposing (sample)
import Time exposing (Month(..), Weekday(..))


{-| Generate a random day of the week.
-}
weekday : Generator Weekday
weekday =
    sample
        [ Mon
        , Tue
        , Wed
        , Thu
        , Fri
        , Sat
        , Sun
        ]
        |> map (Maybe.withDefault Mon)


{-| Generate a random month of the year.
-}
month : Generator Month
month =
    sample
        [ Jan
        , Feb
        , Mar
        , Apr
        , May
        , Jun
        , Jul
        , Aug
        , Sep
        , Oct
        , Nov
        , Dec
        ]
        |> map (Maybe.withDefault Jan)
