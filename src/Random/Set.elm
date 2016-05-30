module Random.Set exposing (..)

{-| List of Random Set Generators

# Generators
@docs empty, singleton, set, notInSet

# Combinators
@docs select, selectWithDefault

-}

import Set exposing (Set)
import Random exposing (Generator, map, andThen)
import Random.Extra exposing (constant, filter)


{-| Generator that always returns the empty set
-}
empty : Generator (Set comparable)
empty =
    constant Set.empty


{-| Generator that creates a singleton set from a generator
-}
singleton : Generator comparable -> Generator (Set comparable)
singleton generator =
    map Set.singleton generator


{-| Filter a generator of all values not in a given set.
-}
notInSet : Set comparable -> Generator comparable -> Generator comparable
notInSet set generator =
    filter (not << flip Set.member set) generator


{-| Select a value from a set uniformly at random, or `Nothing` for an empty set.
Analogous to `Random.Extra.select` but with sets.
-}
select : Set comparable -> Generator (Maybe comparable)
select set =
    Random.Extra.select (Set.toList set)


{-| Select a value from a set uniformly at random, with a default.
Analogous to `Random.Extra.selectWithDefault` but with sets.
-}
selectWithDefault : comparable -> Set comparable -> Generator comparable
selectWithDefault default set =
    Random.Extra.selectWithDefault default (Set.toList set)


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
                    `andThen` \val ->
                                let
                                    newSet =
                                        Set.insert val set
                                in
                                    if Set.size newSet == Set.size set then
                                        helper set remaining (strikes + 1)
                                    else
                                        helper newSet (remaining - 1) 0
    in
        helper Set.empty maxLength 0
