module Random.Task where

import Task         exposing (Task, succeed, fail, sleep)
import Random       exposing (Generator, float)
import Random.Extra exposing (map, constant, flatMap)
import Time         exposing (Time)

task : Generator value -> Generator (Task error value)
task generator =
  map succeed generator

error : Generator error -> Generator (Task error value)
error generator =
  map fail generator


timeout : Time -> Generator (Task error ())
timeout time =
  constant (sleep time)


rangeLengthTimeout : Time -> Time -> Generator (Task error ())
rangeLengthTimeout minTime maxTime =
  flatMap timeout (float minTime maxTime)
