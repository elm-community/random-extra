module Utils exposing (selectUniqByIndexes)

import Array exposing (Array)
import UnionFind exposing (UnionFind)


selectUniqByIndexes : Array a -> List Int -> List a
selectUniqByIndexes values randomIndexes =
    let
        modByLength : Int -> Int
        modByLength =
            modBy (Array.length values)

        step : Int -> ( UnionFind Int, List a ) -> ( UnionFind Int, List a )
        step randomIndex ( uf, acc ) =
            let
                -- Finding corresponding leader of random index
                -- If the index was already used the next one will be got
                -- until not used index will be found
                leaderOfElement =
                    UnionFind.find randomIndex uf

                -- Finding the neighbour index for current one
                -- If the index was already used the next one will be got
                -- until not used index will be found
                leaderOfNextElement =
                    UnionFind.find (modByLength (leaderOfElement + 1)) uf
            in
            case Array.get leaderOfElement values of
                -- theoretically impossible case
                Nothing ->
                    ( uf, acc )

                Just value ->
                    -- Making a union between the index and the neighbour
                    ( UnionFind.union leaderOfElement leaderOfNextElement uf
                    , value :: acc
                    )
    in
    if Array.isEmpty values then
        []

    else
        List.foldr
            step
            ( UnionFind.quickUnionPathCompression, [] )
            randomIndexes
            |> Tuple.second
