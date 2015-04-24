module Random.Result where

import Random exposing (Generator, customGenerator, generate,float)
import Random.Extra exposing (map)


ok : Generator value -> Generator (Result error value)
ok generator =
  map Ok generator


error : Generator error -> Generator (Result error value)
error generator =
  map Err generator


frequency : Int -> Int -> Generator error -> Generator value -> Generator (Result error value)
frequency errFrequency okFrequency errorGenerator okGenerator =
  let
      ratio =
        if | errFrequency == 0 && okFrequency == 0 -> 0.5
           | errFrequency == 0 -> 1
           | okFrequency  == 0 -> 0
           | otherwise ->
              (toFloat okFrequency) /
                (toFloat errFrequency + toFloat okFrequency)
  in
    customGenerator <|
      \seed ->
        let (value, seed1) = generate (float 0 1) seed
        in
          if value < ratio
            then
              let (error, seed2) = generate errorGenerator seed1
              in
                (Err error, seed2)
            else
              let (ok, seed2) = generate okGenerator seed1
              in
                (Ok ok, seed2)
