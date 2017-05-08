module Random.Pcg.Array exposing (..)

{-| Extra randomized functions on arrays.

# Create an Array
@docs array, rangeLengthArray

# Work with an Array
@docs sample, choose, shuffle

-}

import Array exposing (Array, fromList, empty)
import Random.Pcg exposing (Generator, map, list, int, andThen)
import Random.Pcg.Extra exposing (constant)


{-| Generate a random array of given size given a random generator

    randomLength5IntArray = array 5 (int 0 100)
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
            Random.Pcg.int 0 (Array.length arr - 1)
    in
        Random.Pcg.map (\index -> Array.get index arr) gen


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
                Random.Pcg.int 0 lastIndex
        in
            Random.Pcg.map
                (\index ->
                    ( Array.get index arr, Array.append (front index) (back index) )
                )
                gen


{-| Shuffle the array using the Fisher-Yates algorithm. Takes O(_n_ log _n_)
time and O(_n_) additional space.
-}
shuffle : Array a -> Generator (Array a)
shuffle arr =
    if Array.isEmpty arr then
        constant arr
    else
        let
            helper : ( List a, Array a ) -> Generator ( List a, Array a )
            helper ( done, remaining ) =
                choose remaining
                    |> andThen
                        (\( m_val, shorter ) ->
                            case m_val of
                                Nothing ->
                                    constant ( done, shorter )

                                Just val ->
                                    helper ( val :: done, shorter )
                        )
        in
            Random.Pcg.map (Tuple.first >> Array.fromList) (helper ( [], arr ))
