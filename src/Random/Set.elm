module Random.Set exposing (..)

{-| Extra randomized functions on sets.

# Create a Set
@docs set

# Create a Generator
@docs sample

# Modify a Generator
@docs notInSet

-}

import Set exposing (Set)
import Random exposing (Generator, map, andThen)
import Random.Extra exposing (constant, filter)


{-| Filter a generator of all values not in a given set.
-}
notInSet : Set comparable -> Generator comparable -> Generator comparable
notInSet set generator =
    filter (not << flip Set.member set) generator


{-| Select a value from a set uniformly at random, or `Nothing` for an empty set.
Analogous to `Random.Extra.sample` but with sets.
-}
sample : Set comparable -> Generator (Maybe comparable)
sample set =
    Random.Extra.sample (Set.toList set)


{-| Generate a set of at most the given size from a generator.

The size of a generated set is limited both by the integer provided and the
number of unique values the generator can produce. It is very likely, but not
guaranteed, that generated sets will be as big as the smaller of these two limits.
-}
set : Int -> Generator comparable -> Generator (Set comparable)
set maxLength generator =
    let
        helper set remaining strikes =
            if remaining <= 0 || strikes == 10 then
                constant set
            else
                generator
                    |> andThen
                        (\val ->
                            let
                                newSet =
                                    Set.insert val set
                            in
                                if Set.size newSet == Set.size set then
                                    helper set remaining (strikes + 1)
                                else
                                    helper newSet (remaining - 1) 0
                        )
    in
        helper Set.empty maxLength 0
