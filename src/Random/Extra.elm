module Random.Extra where
{-| Module providing extra functionality to the core Random module.

# Constant Generators
@docs constant

# Generator Transformers
@docs flattenList

# Select
@docs select, selectWithDefault, frequency

# Maps
@docs map, map2, map3, map4, map5, map6, mapConstraint

# Flat Maps
@docs flatMap, flatMap2, flatMap3, flatMap4, flatMap5, flatMap6

# Zips
@docs zip, zip3, zip4, zip5, zip6

# Chaining Generators
@docs andMap, andThen

# Filtering Generators
@docs keepIf, dropIf

# Merging Generators
@docs merge

# Generate Functions
@docs quickGenerate, cappedGenerateUntil, generateIterativelyUntil, generateIterativelySuchThat, generateUntil, maybeGenerateSuchThat, generateSuchThat

-}

import Random       exposing (Generator, Seed, generate, customGenerator, list, int, float)
import Random.Bool  exposing (bool)
import Utils        exposing (get)
import List

frequency : List (Float, Generator a) -> Generator a -> Generator a
frequency pairs defaultGenerator =
  let
      frequencies : List Float
      frequencies = List.map (abs << fst) pairs

      total : Float
      total = List.sum frequencies
  in
      if total == 0
      then
        defaultGenerator
      else
        customGenerator <|
          \seed ->
            let
                (randIndex, seed1) = generate (float 0 total) seed

                index = floor randIndex

                maybePair = get index pairs

                generator = case maybePair of
                  Nothing -> defaultGenerator
                  Just (_ , gen) -> gen

            in
                generate generator seed

{-| Convert a generator into a generator that only generates values
that satisfy a given predicate.
Note that if the predicate is unsatisfiable, the generator will not terminate.
-}
keepIf : (a -> Bool) -> Generator a -> Generator a
keepIf predicate generator =
  let
      gen seed =
        let
            (value, seed1) = generate generator seed
        in
            if predicate value
            then
              (value, seed1)
            else
              gen seed1
  in
      customGenerator gen


{-| Convert a generator into a generator that only generates values
that do not satisfy a given predicate.
-}
dropIf : (a -> Bool) -> Generator a -> Generator a
dropIf predicate =
  keepIf (\a -> not (predicate a))


{-| Turn a list of generators into a generator of lists.
-}
flattenList : List (Generator a) -> Generator (List a)
flattenList generators = case generators of
  [] -> constant []
  g :: gs ->
    customGenerator <|
      \seed ->
        let (first, seed1)  = generate g seed
            (rest , seed2)  = generate (flattenList gs) seed1
        in
            (first :: rest, seed2)


{-| Generator that randomly selects an element from a list.
-}
select : List a -> Generator (Maybe a)
select list =
  customGenerator <|
    (\seed ->
        let (index, nextSeed) = generate (int 0 (List.length list - 1)) seed
        in
          (get index list, nextSeed))


{-| Generator that randomly selects an element from a list with a default value
(in case you pass in an empty list).
-}
selectWithDefault : a -> List a -> Generator a
selectWithDefault defaultValue list =
  customGenerator <|
    (\seed ->
        let (index, nextSeed) = generate (int 0 (List.length list - 1)) seed
        in
          case get index list of
            Nothing     -> (defaultValue, nextSeed)
            Just value  -> (value, nextSeed))




{-| Create a generator that always returns the same value.
-}
constant : a -> Generator a
constant value =
  customGenerator
    (\seed ->
        let (_, seed1) = generate (int 0 1) seed
        in
          (value, seed1))



andMap : Generator (a -> b) -> Generator a -> Generator b
andMap funcGenerator generator =
  customGenerator <|
    (\seed ->
        let (f, seed1) = generate funcGenerator seed
            (a, seed2) = generate generator seed1
        in
          ((f a), seed2))



zip : Generator a -> Generator b -> Generator (a, b)
zip = map2 (,)

zip3 : Generator a -> Generator b -> Generator c -> Generator (a, b, c)
zip3 = map3 (,,)

zip4 : Generator a -> Generator b -> Generator c -> Generator d -> Generator (a, b, c, d)
zip4 = map4 (,,,)

zip5 : Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator (a, b, c, d, e)
zip5 = map5 (,,,,)

zip6 : Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator (a, b, c, d, e, f)
zip6 = map6 (,,,,,)


andThen : Generator a -> (a -> Generator b) -> Generator b
andThen generator constructor =
  flatMap constructor generator

flatMap : (a -> Generator b) -> Generator a -> Generator b
flatMap constructor generator =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generator seed
            generatorB = constructor valueA
        in
          generate generatorB seed1)

flatMap2 : (a -> b -> Generator c) -> Generator a -> Generator b -> Generator c
flatMap2 constructor generatorA generatorB =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            generatorC = constructor valueA valueB
        in
          generate generatorC seed2)

flatMap3 : (a -> b -> c -> Generator d) -> Generator a -> Generator b -> Generator c -> Generator d
flatMap3 constructor generatorA generatorB generatorC =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            generatorD = constructor valueA valueB valueC
        in
          generate generatorD seed3)

flatMap4 : (a -> b -> c -> d -> Generator e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
flatMap4 constructor generatorA generatorB generatorC generatorD =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            generatorE = constructor valueA valueB valueC valueD
        in
          generate generatorE seed4)


flatMap5 : (a -> b -> c -> d -> e -> Generator f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
flatMap5 constructor generatorA generatorB generatorC generatorD generatorE =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
            generatorF = constructor valueA valueB valueC valueD valueE
        in
          generate generatorF seed5)


flatMap6 : (a -> b -> c -> d -> e -> f -> Generator g) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g
flatMap6 constructor generatorA generatorB generatorC generatorD generatorE generatorF =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
            (valueF, seed6) = generate generatorF seed5
            generatorG = constructor valueA valueB valueC valueD valueE valueF
        in
          generate generatorG seed6)



map : (a -> b) -> Generator a -> Generator b
map f generator =
  customGenerator
    (\seed ->
        let (value, nextSeed) = generate generator seed
        in
          (f value, nextSeed))

map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 f generatorA generatorB =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
        in
          (f valueA valueB, seed2))

map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 f generatorA generatorB generatorC =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
        in
          (f valueA valueB valueC, seed3))

map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 f generatorA generatorB generatorC generatorD =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
        in
          (f valueA valueB valueC valueD, seed4))

map5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
map5 f generatorA generatorB generatorC generatorD generatorE =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
        in
          (f valueA valueB valueC valueD valueE, seed5))

map6 : (a -> b -> c -> d -> e -> f -> g) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g
map6 f generatorA generatorB generatorC generatorD generatorE generatorF =
  customGenerator
    (\seed ->
        let (valueA, seed1) = generate generatorA seed
            (valueB, seed2) = generate generatorB seed1
            (valueC, seed3) = generate generatorC seed2
            (valueD, seed4) = generate generatorD seed3
            (valueE, seed5) = generate generatorE seed4
            (valueF, seed6) = generate generatorF seed5
        in
          (f valueA valueB valueC valueD valueE valueF, seed6))

merge : Generator a -> Generator a -> Generator a
merge generator1 generator2 =
  customGenerator
    (\seed ->
        let value = quickGenerate bool seed
        in
          if value == True
          then
            generate generator1 seed
          else
            generate generator2 seed)


generateSuchThat : (a -> Bool) -> Generator a -> Seed -> (a, Seed)
generateSuchThat predicate generator seed =
  let (value, nextSeed) = generate generator seed
  in
    if predicate value
    then
      (value, nextSeed)
    else
      generateSuchThat predicate generator nextSeed

maybeGenerateSuchThat : Int -> (a -> Bool) -> Generator a -> Seed -> Maybe (a, Seed)
maybeGenerateSuchThat numberOfTries predicate generator seed =
  if numberOfTries <= 0
  then Nothing
  else
    let (value, nextSeed) = generate generator seed
    in
      if predicate value
      then
        Just (value, nextSeed)
      else
        maybeGenerateSuchThat (numberOfTries - 1) predicate generator nextSeed


generateUntil : (a -> Bool) -> Generator a -> Seed -> List a
generateUntil predicate generator seed =
  let (value, nextSeed) = generate generator seed
  in
    if predicate value
    then
      value :: generateUntil predicate generator nextSeed
    else
      []


generateIterativelySuchThat : Int -> (a -> Bool) -> (Int -> Generator a) -> Seed -> List a
generateIterativelySuchThat maxLength predicate constructor seed =
  let notPredicate = (\value -> not (predicate value))

      iterate index =
        if index >= maxLength
        then
          []
        else
          (generateUntil notPredicate (constructor index) seed) `List.append`
          (iterate (index + 1))

  in
    iterate 0


generateIterativelyUntil : Int -> (a -> Bool) -> (Int -> Generator a) -> Seed -> List a
generateIterativelyUntil maxLength predicate constructor seed =
  let iterate index =
        if index >= maxLength
        then
          []
        else
          (generateUntil predicate (constructor index) seed) `List.append`
          (iterate (index + 1))

  in
    iterate 0


cappedGenerateUntil : Int -> (a -> Bool) -> Generator a -> Seed -> List a
cappedGenerateUntil maxGenerations predicate generator seed =
  if maxGenerations <= 0
  then
    []
  else
    let (value, nextSeed) = generate generator seed
    in
      if predicate value
      then
        value :: cappedGenerateUntil (maxGenerations - 1) predicate generator nextSeed
      else
        []


generateWithDefault : List a -> Generator a -> Seed -> List (a, Seed)
generateWithDefault list generator seed =
  case list of
    [] -> [generate generator seed]
    x :: xs ->
      let (value, nextSeed) = generate generator seed
      in
        (x, nextSeed) :: generateWithDefault xs generator nextSeed

quickGenerate : Generator a -> Seed -> a
quickGenerate generator seed =
  (fst (generate generator seed))

mapConstraint : (a -> b) -> Generator a -> Generator (a, b)
mapConstraint constraint generator =
  customGenerator
    (\seed ->
        let (value, seed1) = generate generator seed
        in
          ((value, constraint value), seed1))
