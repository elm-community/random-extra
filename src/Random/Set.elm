module Random.Set where
{-| List of Random Set Generators

-}

import Set          exposing (Set)
import Random       exposing (Generator)
import Random.Extra exposing (constant, map, dropIf, keepIf)


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


{-| A generator that creates values not present in a given set.
-}
notInSet : Set comparable -> Generator comparable -> Generator comparable
notInSet set generator =
  dropIf (flip Set.member set) generator

{-| A generator that creates values from a given set.
-}
inSet : Set comparable -> Generator comparable -> Generator comparable
inSet set generator =
  keepIf (flip Set.member set) generator

--set : Int -> Generator comparable -> Generator (Set comparable)
--set maxLength generator =
