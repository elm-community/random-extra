module ListTests exposing (suite)

import Expect
import Fuzz
import Random
import Random.List
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

                    ( shuffledList, _ ) =
                        Random.step (Random.List.shuffle initialList) (Random.initialSeed 0)
                in
                shuffledList
                    |> Expect.equalLists [ 4, 8, 5, 1, 0, 9, 7, 6, 2, 3 ]

        --
        , Test.fuzz (Fuzz.tuple ( Fuzz.int, Fuzz.intRange 0 100 )) "test existing all of the shuffled elements" <|
            \( seedRoot, listLength ) ->
                let
                    initialList =
                        List.range 0 listLength

                    ( shuffledList, _ ) =
                        Random.step (Random.List.shuffle initialList) (Random.initialSeed seedRoot)
                in
                shuffledList
                    |> List.sort
                    |> Expect.equalLists initialList

        --
        , Test.fuzz (Fuzz.tuple ( Fuzz.int, Fuzz.intRange 0 100 )) "test uniq of the shuffled elements" <|
            \( seedRoot, listLength ) ->
                let
                    initialList =
                        List.range 0 listLength

                    ( shuffledList, _ ) =
                        Random.step (Random.List.shuffle initialList) (Random.initialSeed seedRoot)
                in
                shuffledList
                    |> Set.fromList
                    |> Set.diff (Set.fromList initialList)
                    |> Expect.equalSets Set.empty

        --
        , Test.fuzzWith { runs = 1 } Fuzz.int "critical 100k length" <|
            \seedRoot ->
                let
                    initialList =
                        List.range 0 100000

                    ( shuffledList, _ ) =
                        Random.step (Random.List.shuffle initialList) (Random.initialSeed seedRoot)
                in
                shuffledList
                    |> List.sort
                    |> Expect.equalLists initialList
        ]
