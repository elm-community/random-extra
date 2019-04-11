module Tests.Random.Array exposing (suite)

import Array
import Expect
import Fuzz
import Random
import Random.Array
import Set
import Test exposing (Test)


suite : Test
suite =
    Test.describe "shuffle"
        [ Test.test "shuffle 10 elements with a given seedRoot" <|
            \() ->
                let
                    initialList =
                        List.range 0 9

                    ( shuffledArray, _ ) =
                        Random.step (Random.Array.shuffle (Array.fromList initialList)) (Random.initialSeed 0)
                in
                shuffledArray
                    |> Array.toList
                    |> Expect.equalLists [ 4, 8, 5, 1, 0, 9, 7, 6, 2, 3 ]

        --
        , Test.fuzz (Fuzz.tuple ( Fuzz.int, Fuzz.intRange 0 100 )) "test existing all of the shuffled elements" <|
            \( seedRoot, listLength ) ->
                let
                    initialList =
                        List.range 0 listLength

                    ( shuffledArray, _ ) =
                        Random.step (Random.Array.shuffle (Array.fromList initialList)) (Random.initialSeed seedRoot)
                in
                shuffledArray
                    |> Array.toList
                    |> List.sort
                    |> Expect.equalLists initialList

        --
        , Test.fuzz (Fuzz.tuple ( Fuzz.int, Fuzz.intRange 0 100 )) "test uniq of the shuffled elements" <|
            \( seedRoot, listLength ) ->
                let
                    initialList =
                        List.range 0 listLength

                    ( shuffledArray, _ ) =
                        Random.step (Random.Array.shuffle (Array.fromList initialList)) (Random.initialSeed seedRoot)
                in
                shuffledArray
                    |> Array.toList
                    |> Set.fromList
                    |> Set.diff (Set.fromList initialList)
                    |> Expect.equalSets Set.empty

        --
        , Test.fuzzWith { runs = 1 } Fuzz.int "critical 100k length" <|
            \seedRoot ->
                let
                    initialList =
                        List.range 0 100000

                    ( shuffledArray, _ ) =
                        Random.step (Random.Array.shuffle (Array.fromList initialList)) (Random.initialSeed seedRoot)
                in
                shuffledArray
                    |> Array.toList
                    |> List.sort
                    |> Expect.equalLists initialList
        ]
