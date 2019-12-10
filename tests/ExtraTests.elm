module ExtraTests exposing (suite)

import Expect
import Fuzz
import Random
import Random.Extra
import Test exposing (Test)


{-| This library did not originally have tests, so the tests below have been added with the functions
that they test.

If you want to add tests, please open a PR! Even so-so tests are better than no tests.

-}
suite : Test
suite =
    Test.describe "Random.Extra.sequence"
        [ Test.test "it runs the generators in the correct order" <|
            \() ->
                [ Random.constant 0, Random.constant 1 ]
                    |> Random.Extra.sequence
                    |> (\gen -> Random.step gen (Random.initialSeed 42))
                    |> Tuple.first
                    |> Expect.equalLists [ 0, 1 ]
        ]
