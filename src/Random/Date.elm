module Random.Date where
{-| List of date Generators

# Generators
@docs date, day, month, year, hour, hour24, hour12, minute, second

-}

import Date exposing (Day(..), Month(..), fromTime, toTime, Date)
import Time exposing (Time)
import Random exposing (..)
import Random.Extra exposing (map)

{-| Generate a random day of the week.
-}
day : Generator Day
day =
  let intToDay int =
        case int of
          0 -> Mon
          1 -> Tue
          2 -> Wed
          3 -> Thu
          4 -> Fri
          5 -> Sat
          _ -> Sun
  in
    map intToDay (int 0 6)

{-| Generate a random month of the year.
-}
month : Generator Month
month =
  let intToMonth int =
        case int of
          0   -> Jan
          1   -> Feb
          2   -> Mar
          3   -> Apr
          4   -> May
          5   -> Jun
          6   -> Jul
          7   -> Aug
          8   -> Sep
          9   -> Oct
          10  -> Nov
          _   -> Dec
  in
    map intToMonth (int 0 11)

{-| Generate a random year given a start year and end year (alias for `int`)
-}
year : Int -> Int -> Generator Int
year = int

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
