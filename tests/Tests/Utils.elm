module Tests.Utils exposing (suite)

import Array
import Expect
import Test exposing (Test)
import Utils


suite : Test
suite =
    Test.describe "selectUniqByIndexes"
        [ Test.test "empty values and indexes" <|
            \() ->
                Utils.selectUniqByIndexes Array.empty []
                    |> Expect.equalLists []

        --
        , Test.test "empty values and some indexes" <|
            \() ->
                Utils.selectUniqByIndexes Array.empty [ 0, 1 ]
                    |> Expect.equalLists []

        --
        , Test.test "some values and empty indexes" <|
            \() ->
                Utils.selectUniqByIndexes (Array.fromList [ 0, 1 ]) []
                    |> Expect.equalLists []

        --
        , Test.test "select 2 elements in order" <|
            \() ->
                Utils.selectUniqByIndexes (Array.fromList [ 0, 1 ]) [ 0, 1 ]
                    |> Expect.equalLists [ 0, 1 ]

        --
        , Test.test "select 2 elements in reverse order" <|
            \() ->
                Utils.selectUniqByIndexes (Array.fromList [ 0, 1 ]) [ 1, 0 ]
                    |> Expect.equalLists [ 1, 0 ]

        --
        , Test.test "select 2 elements by same indexes" <|
            \() ->
                Utils.selectUniqByIndexes (Array.fromList [ 0, 1 ]) [ 0, 0 ]
                    |> Expect.equalLists [ 1, 0 ]

        --
        , Test.test "select 2 of 10 elements" <|
            \() ->
                Utils.selectUniqByIndexes (Array.fromList (List.range 0 9)) [ 4, 8 ]
                    |> Expect.equalLists [ 4, 8 ]

        --
        , Test.test "select all of 10 elements" <|
            \() ->
                Utils.selectUniqByIndexes (Array.fromList (List.range 0 9)) [ 4, 8, 1, 3, 4, 9, 1, 0, 5, 2 ]
                    |> Expect.equalLists [ 7, 8, 6, 3, 4, 9, 1, 0, 5, 2 ]

        --
        , Test.test "select all of 10 elements by same index" <|
            \() ->
                Utils.selectUniqByIndexes (Array.fromList (List.range 0 9)) [ 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 ]
                    |> Expect.equalLists [ 4, 3, 2, 1, 0, 9, 8, 7, 6, 5 ]
        ]
