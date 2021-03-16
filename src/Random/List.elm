module Random.List exposing (choose, shuffle, choices)

{-| Extra randomized functions on lists.


# Work with a List

@docs choose, shuffle, choices

-}

import Random exposing (Generator, andThen, constant)


{-| Get nth element of the list. If the list is empty, the selected element
will be `Nothing`.
-}
get : Int -> List a -> Maybe a
get index list =
    list
        |> List.drop index
        |> List.head


{-| Sample without replacement: produce a randomly selected element of the
list, and the list with that element omitted. If the list is empty, the
selected element will be `Nothing`.
-}
choose : List a -> Generator ( Maybe a, List a )
choose list =
    if List.isEmpty list then
        constant ( Nothing, list )

    else
        let
            lastIndex =
                List.length list - 1

            front i =
                List.take i list

            back i =
                List.drop (i + 1) list

            gen =
                Random.int 0 lastIndex
        in
        Random.map
            (\index ->
                ( get index list, List.append (front index) (back index) )
            )
            gen


{-| Repeated sample without replacement: produce a list of randomly
selected elements of some list, and the list of unselected elements.
-}
choices : Int -> List a -> Generator ( List a, List a )
choices count list =
    if count < 1 then
        constant ( [], list )

    else
        choose list
            |> andThen
                (\( choice, remaining ) ->
                    let
                        genRest =
                            Random.lazy (\_ -> choices (count - 1) remaining)

                        addToChoices =
                            \elem ( chosen, unchosen ) -> ( elem :: chosen, unchosen )
                    in
                    case choice of
                        Nothing ->
                            constant ( [], list )

                        Just elem ->
                            Random.map (addToChoices elem) genRest
                )


anyInt : Generator Int
anyInt =
    Random.int Random.minInt Random.maxInt


{-| Shuffle the list. Takes O(_n_ log _n_) time and no extra space.
-}
shuffle : List a -> Generator (List a)
shuffle list =
    Random.map
        (\independentSeed ->
            list
                |> List.foldl
                    (\item ( acc, seed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step anyInt seed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], independentSeed )
                |> Tuple.first
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
        )
        Random.independentSeed
