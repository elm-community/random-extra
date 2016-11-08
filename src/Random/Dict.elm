module Random.Dict exposing (..)

{-| Extra randomized functions on dicts.

# Generators
@docs dict, rangeLengthDict

-}

import Dict exposing (Dict, fromList)
import Random exposing (Generator, map, pair, list, int, andThen)


{-| Generate a random dict with given length, key generator, and value generator

    randomLength10StringIntDict = dict 10 (englishWord 10) (int 0 100)
-}
dict : Int -> Generator comparable -> Generator value -> Generator (Dict comparable value)
dict dictLength keyGenerator valueGenerator =
    map (fromList) (list dictLength (pair keyGenerator valueGenerator))


{-| Generate a random dict of random length given a minimum length and
a maximum length.
-}
rangeLengthDict : Int -> Int -> Generator comparable -> Generator value -> Generator (Dict comparable value)
rangeLengthDict minLength maxLength keyGenerator valueGenerator =
    andThen (\len -> dict len keyGenerator valueGenerator) (int minLength maxLength)
