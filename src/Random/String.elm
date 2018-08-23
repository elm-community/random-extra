module Random.String exposing (rangeLengthString, string)

{-| Extra randomized functions on strings.


# Create a String

@docs string, rangeLengthString

-}

import Random exposing (Generator, andThen, int, list, map, map2)
import String exposing (fromList)


{-| Generate a random string of a given length with a given character generator

    fiveLetterEnglishWord =
        string 5 Random.Char.english

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
