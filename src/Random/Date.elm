module Random.Date exposing (..)

{-| Extra randomized functions on dates.

# Generators
@docs date, day, month, year, hour, hour24, hour12, minute, second

-}

import Date exposing (Day(..), Month(..), fromTime, toTime, Date)
import Time exposing (Time)
import Random exposing (Generator, map, int, float)
import Random.Extra exposing (sample)


{-| Generate a random day of the week.
-}
day : Generator Day
day =
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


{-| Generate a random year given a start year and end year (alias for `int`)
-}
year : Int -> Int -> Generator Int
year =
    int


{-| Generate a random hour (random int between 0 and 23 inclusive)
-}
hour : Generator Int
hour =
    int 0 23


{-| Generate a random 24-hour day hour (random int between 0 and 23 inclusive)
-}
hour24 : Generator Int
hour24 =
    int 0 23


{-| Generate a random 12-hour day hour (random int between 0 and 11 inclusive)
-}
hour12 : Generator Int
hour12 =
    int 0 11


{-| Generate a random minute (random int between 0 and 59 inclusive)
-}
minute : Generator Int
minute =
    int 0 59


{-| Generate a random second (random int between 0 and 59 inclusive)
-}
second : Generator Int
second =
    int 0 59


{-| Generate a random date given a start date and an end date.
-}
date : Date -> Date -> Generator Date
date startDate endDate =
    map fromTime (float (toTime startDate) (toTime endDate))
