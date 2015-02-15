module Random.Array where
{-| List of Array Generators

# Generators
@docs array, emptyArray, rangeLengthArray

-}

import Array (Array, fromList, empty)
import Random (..)
import Random.Extra (map, flatMap, constant)

{-| Generate a random array of given size given a random generator

    randomLength5IntArray = array 5 (int 0 100)
-}
array : Int -> Generator a -> Generator (Array a)
array arrayLength generator =
  map fromList (list arrayLength generator)

{-| Generator that always generates the empty array
-}
emptyArray : Generator (Array a)
emptyArray =
  constant empty

{-| Generate a random array of random length given a minimum length and
a maximum length.
-}
rangeLengthArray : Int -> Int -> Generator a -> Generator (Array a)
rangeLengthArray minLength maxLength generator =
  flatMap (\len -> array len generator) (int minLength maxLength) 
