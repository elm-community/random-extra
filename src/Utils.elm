module Utils exposing (shuffleByIndexes)

import Array exposing (Array)
import UnionFind


shuffleByIndexes : Array a -> List Int -> List a
shuffleByIndexes values randomIndexes =
    let
        modByLength =
            modBy (Array.length values)

        ( _, shuffledListOfValues ) =
            List.foldr
                (\randomIndex ( uf, acc ) ->
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
                )
                ( UnionFind.quickUnionPathCompression, [] )
                randomIndexes
    in
    shuffledListOfValues
