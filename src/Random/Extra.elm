module Random.Extra exposing
    ( bool
    , map6, andMap
    , oneIn, maybe, result, choice
    , sequence, traverse, choices, frequency, sample, combine, rangeLengthList
    , filter
    , andThen2, andThen3, andThen4, andThen5, andThen6
    )

{-| This module provides many common and general-purpose helper functions for
core's Random library. You can find even more useful functions for a particular
type in the other modules.


# Values

@docs bool


# Maps

For `map` and `mapN` up through N=5, use the core library.

@docs map6, andMap


# New Generators

@docs oneIn, maybe, result, choice


# Working with Lists

@docs sequence, traverse, choices, frequency, sample, combine, rangeLengthList


# Filtered Generators

@docs filter


# andThenN

These functions are like `mapN` except the function you pass in does not return
an exact value, but instead another generator. That means you can take in several
random arguments to drive more randomness.

@docs andThen2, andThen3, andThen4, andThen5, andThen6

-}

import Random exposing (Generator, andThen, constant, float, int, list, map)


{-| An unbiased generator of `Bool` values.
-}
bool : Generator Bool
bool =
    Random.uniform True [ False ]


{-| Map a function of six arguments over six generators.
-}
map6 : (a -> b -> c -> d -> e -> f -> g) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g
map6 f generatorA generatorB generatorC generatorD generatorE generatorF =
    Random.map5 f generatorA generatorB generatorC generatorD generatorE |> andMap generatorF


{-| Map over any number of generators.

    type alias Person = -- some large record

    randomPerson : Generator Person
    randomPerson =
      map Person genFirstName
        |> andMap genLastName
        |> andMap genBirthday
        |> andMap genPhoneNumber
        |> andMap genAddress
        |> andMap genEmail

-}
andMap : Generator a -> Generator (a -> b) -> Generator b
andMap =
    Random.map2 (|>)


{-| Filter a generator so that all generated values satisfy the given predicate.

    evens : Generator Int
    evens =
        filter (\i -> i % 2 == 0) (int minInt maxInt)

**Warning:** If the predicate is unsatisfiable, the generator will not
terminate, your application will crash with a stack overflow, and you will be
sad. You should also avoid predicates that are merely very difficult to satisfy.

    badCrashingGenerator =
        filter (\_ -> False) anotherGenerator

    likelyCrashingGenerator =
        filter (\i -> i % 2000 == 0) (int minInt maxInt)

-}
filter : (a -> Bool) -> Generator a -> Generator a
filter predicate generator =
    generator
        |> andThen
            (\a ->
                if predicate a then
                    constant a

                else
                    filter predicate generator
            )


{-| Produce `True` one-in-n times on average.

Do not pass a value less then one to this function.

    flippedHeads =
        oneIn 2

    rolled6 =
        oneIn 6

-}
oneIn : Int -> Generator Bool
oneIn n =
    map ((==) 1) (int 1 n)


{-| Choose between two values with equal probability.

    type Flip
        = Heads
        | Tails

    coinFlip : Generator Flip
    coinFlip =
        choice Heads Tails

Note that this function takes values, not generators. That's because it's meant
to be a lightweight helper for a specific use. If you need to choose between two
generators, use `choices [gen1, gen2]`.

-}
choice : a -> a -> Generator a
choice x y =
    map
        (\b ->
            if b then
                x

            else
                y
        )
        bool


{-| Start with a list of generators, and turn them into a generator that returns a list.
-}
sequence : List (Generator a) -> Generator (List a)
sequence =
    List.foldr (Random.map2 (::)) (Random.constant [])


{-| Apply a function that returns a generator to each element of a list,
and turn it into a generator that returns a list.
-}
traverse : (a -> Generator b) -> List a -> Generator (List b)
traverse f =
    sequence << List.map f


{-| Create a generator that chooses a generator from a list of generators
with equal probability.

We guarantee a nonempty list is passed by splitting it into two arguments.

-}
choices : Generator a -> List (Generator a) -> Generator a
choices hd gens =
    frequency ( 1, hd ) <| List.map (\g -> ( 1, g )) gens


{-| Create a generator that chooses a generator from a list of generators
based on the provided weight. The likelihood of a given generator being
chosen is its weight divided by the total weight (which doesn't have to equal 1).

We guarantee a nonempty list is passed by splitting it into two arguments.

-}
frequency : ( Float, Generator a ) -> List ( Float, Generator a ) -> Generator a
frequency head pairs =
    let
        total =
            List.sum <| List.map (abs << Tuple.first) (head :: pairs)

        pick someChoices n =
            case someChoices of
                ( k, g ) :: rest ->
                    if n <= k then
                        g

                    else
                        pick rest (n - k)

                -- this should never happen
                _ ->
                    Tuple.second head
    in
    float 0 total |> andThen (pick (head :: pairs))


{-| Turn a list of generators into a generator of lists.
-}
combine : List (Generator a) -> Generator (List a)
combine generators =
    case generators of
        [] ->
            constant []

        g :: gs ->
            Random.map2 (::) g (combine gs)


{-| Given a list, choose an element uniformly at random. `Nothing` is only
produced if the list is empty.

    type Direction
        = North
        | South
        | East
        | West

    direction : Generator Direction
    direction =
        sample [ North, South, East, West ]
            |> map (Maybe.withDefault North)

-}
sample : List a -> Generator (Maybe a)
sample =
    let
        find k ys =
            case ys of
                [] ->
                    Nothing

                z :: zs ->
                    if k == 0 then
                        Just z

                    else
                        find (k - 1) zs
    in
    \xs -> map (\i -> find i xs) (int 0 (List.length xs - 1))


{-| Produce `Just` a value on `True`, and `Nothing` on `False`.

You can use `bool` or `oneIn n` for the first argument.

-}
maybe : Generator Bool -> Generator a -> Generator (Maybe a)
maybe genBool genA =
    genBool
        |> andThen
            (\b ->
                if b then
                    map Just genA

                else
                    constant Nothing
            )


{-| Produce an `Ok` a value on `True`, and an `Err` value on `False`.

You can use `bool` or `oneIn n` for the first argument.

-}
result : Generator Bool -> Generator err -> Generator val -> Generator (Result err val)
result genBool genErr genVal =
    genBool
        |> andThen
            (\b ->
                if b then
                    map Ok genVal

                else
                    map Err genErr
            )


{-| Generate a random list of random length given a minimum length and
a maximum length.
-}
rangeLengthList : Int -> Int -> Generator a -> Generator (List a)
rangeLengthList minLength maxLength generator =
    andThen (\len -> list len generator) (int minLength maxLength)


{-| -}
andThen2 : (a -> b -> Generator c) -> Generator a -> Generator b -> Generator c
andThen2 constructor generatorA generatorB =
    generatorA
        |> andThen
            (\a ->
                generatorB
                    |> andThen
                        (\b ->
                            constructor a b
                        )
            )


{-| -}
andThen3 : (a -> b -> c -> Generator d) -> Generator a -> Generator b -> Generator c -> Generator d
andThen3 constructor generatorA generatorB generatorC =
    generatorA
        |> andThen
            (\a ->
                generatorB
                    |> andThen
                        (\b ->
                            generatorC
                                |> andThen
                                    (\c ->
                                        constructor a b c
                                    )
                        )
            )


{-| -}
andThen4 : (a -> b -> c -> d -> Generator e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
andThen4 constructor generatorA generatorB generatorC generatorD =
    generatorA
        |> andThen
            (\a ->
                generatorB
                    |> andThen
                        (\b ->
                            generatorC
                                |> andThen
                                    (\c ->
                                        generatorD
                                            |> andThen
                                                (\d ->
                                                    constructor a b c d
                                                )
                                    )
                        )
            )


{-| -}
andThen5 : (a -> b -> c -> d -> e -> Generator f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
andThen5 constructor generatorA generatorB generatorC generatorD generatorE =
    generatorA
        |> andThen
            (\a ->
                generatorB
                    |> andThen
                        (\b ->
                            generatorC
                                |> andThen
                                    (\c ->
                                        generatorD
                                            |> andThen
                                                (\d ->
                                                    generatorE
                                                        |> andThen
                                                            (\e ->
                                                                constructor a b c d e
                                                            )
                                                )
                                    )
                        )
            )


{-| -}
andThen6 : (a -> b -> c -> d -> e -> f -> Generator g) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g
andThen6 constructor generatorA generatorB generatorC generatorD generatorE generatorF =
    generatorA
        |> andThen
            (\a ->
                generatorB
                    |> andThen
                        (\b ->
                            generatorC
                                |> andThen
                                    (\c ->
                                        generatorD
                                            |> andThen
                                                (\d ->
                                                    generatorE
                                                        |> andThen
                                                            (\e ->
                                                                generatorF
                                                                    |> andThen
                                                                        (\f ->
                                                                            constructor a b c d e f
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )
