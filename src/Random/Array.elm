module Random.Array exposing
    ( array, rangeLengthArray
    , sample, choose, shuffle
    )

{-| Extra randomized functions on arrays.


# Create an Array

@docs array, rangeLengthArray


# Work with an Array

@docs sample, choose, shuffle

-}

import Array exposing (Array, empty, fromList)
import Random exposing (Generator, andThen, constant, int, list, map)
import Utils


{-| Generate a random array of given size given a random generator

    randomLength5IntArray =
        array 5 (int 0 100)

-}
array : Int -> Generator a -> Generator (Array a)
array arrayLength generator =
    map fromList (list arrayLength generator)


{-| Generate a random array of random length given a minimum length and
a maximum length.
-}
rangeLengthArray : Int -> Int -> Generator a -> Generator (Array a)
rangeLengthArray minLength maxLength generator =
    andThen (\len -> array len generator) (int minLength maxLength)


{-| Sample with replacement: produce a randomly selected element of the
array, or `Nothing` for an empty array. Takes O(1) time.
-}
sample : Array a -> Generator (Maybe a)
sample arr =
    let
        gen =
            Random.int 0 (Array.length arr - 1)
    in
    Random.map (\index -> Array.get index arr) gen


{-| Sample without replacement: produce a randomly selected element of the
array, and the array with that element omitted (shifting all later elements
down). If the array is empty, the selected element will be `Nothing`.
-}
choose : Array a -> Generator ( Maybe a, Array a )
choose arr =
    if Array.isEmpty arr then
        constant ( Nothing, arr )

    else
        let
            lastIndex =
                Array.length arr - 1

            front i =
                Array.slice 0 i arr

            back i =
                if
                    i == lastIndex
                    -- workaround for #1
                then
                    Array.empty

                else
                    Array.slice (i + 1) (lastIndex + 1) arr

            gen =
                Random.int 0 lastIndex
        in
        Random.map
            (\index ->
                ( Array.get index arr, Array.append (front index) (back index) )
            )
            gen


{-| Shuffle the list using the Union-Find data structure with path compression algorithm.
Takes O(_n_ log _n_).
-}
shuffle : Array a -> Generator (Array a)
shuffle values =
    let
        -- Yes, it takes O(1) time for get the Array's length,
        -- but let's just keep the value here
        length =
            Array.length values
    in
    Random.map
        (Array.fromList << Utils.selectUniqByIndexes values)
        -- It generates the sequence of random indexes
        -- The indexes could and will (for sure) duplicate each other
        -- But UnionFind will help us to convert them into uniq,
        -- even if all of the indexes will be the same value
        (Random.list length (Random.int 0 (length - 1)))
