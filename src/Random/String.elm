module Random.String exposing (..)

{-| Extra randomized functions on strings.

# Create a String
@docs string, rangeLengthString

-}

import String exposing (fromList)
import Random exposing (Generator, map, map2, list, int, andThen)


{-| Generate a random string of a given length with a given character generator

    fiveLetterEnglishWord = string 5 Random.Char.english
-}
string : Int -> Generator Char -> Generator String
string stringLength charGenerator =
    map fromList (list stringLength charGenerator)


{-| Generates a random string of random length given the minimum length
and maximum length and a given character generator.
-}
rangeLengthString : Int -> Int -> Generator Char -> Generator String
rangeLengthString minLength maxLength charGenerator =
    andThen (\len -> string len charGenerator) (int minLength maxLength)
