module Random.Set exposing (notInSet, sample, set)

{-| Extra randomized functions on sets.


# Create a Set

@docs set


# Create a Generator

@docs sample


# Modify a Generator

@docs notInSet

-}

import Random exposing (Generator, andThen, constant, map)
import Random.Extra exposing (filter)
import Set exposing (Set)


{-| Filter a generator of all values not in a given set.
-}
notInSet : Set comparable -> Generator comparable -> Generator comparable
notInSet aSet generator =
    filter (\element -> not <| Set.member element aSet) generator


{-| Select a value from a set uniformly at random, or `Nothing` for an empty set.
Analogous to `Random.Extra.sample` but with sets.
-}
sample : Set comparable -> Generator (Maybe comparable)
sample aSet =
    Random.Extra.sample (Set.toList aSet)


{-| Generate a set of at most the given size from a generator.

The size of a generated set is limited both by the integer provided and the
number of unique values the generator can produce. It is very likely, but not
guaranteed, that generated sets will be as big as the smaller of these two limits.

-}
set : Int -> Generator comparable -> Generator (Set comparable)
set maxLength generator =
    let
        helper aSet remaining strikes =
            if remaining <= 0 || strikes == 10 then
                constant aSet

            else
                generator
                    |> andThen
                        (\val ->
                            let
                                newSet =
                                    Set.insert val aSet
                            in
                            if Set.size newSet == Set.size aSet then
                                helper aSet remaining (strikes + 1)

                            else
                                helper newSet (remaining - 1) 0
                        )
    in
    helper Set.empty maxLength 0
