module Tests.Random.List exposing (suite)

import Expect
import Fuzz
import Random
import Random.List
import Set
import Test exposing (Test)


suite : Test
suite =
    Test.describe "shuffle"
        [ Test.test "shuffle 10 elements with given seedRoot" <|
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
        , Test.fuzzWith { runs = 1 } Fuzz.int "test existing all of the 10k elements" <|
            \seedRoot ->
                let
                    initialList =
                        List.range 0 9999

                    ( shuffledList, _ ) =
                        Random.step (Random.List.shuffle initialList) (Random.initialSeed seedRoot)
                in
                List.sort shuffledList
                    |> Expect.equalLists initialList

        --
        , Test.fuzzWith { runs = 1 } Fuzz.int "test uniq of 10k elements" <|
            \seedRoot ->
                let
                    initialList =
                        List.range 0 9999

                    ( shuffledList, _ ) =
                        Random.step (Random.List.shuffle initialList) (Random.initialSeed seedRoot)
                in
                Set.diff (Set.fromList initialList) (Set.fromList shuffledList)
                    |> Expect.equalSets Set.empty
        ]
