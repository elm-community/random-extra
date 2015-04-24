module Random.Task where

import Task         exposing (Task, ThreadID, spawn, succeed, fail, sleep)
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


threadedTask : Generator (Task error value) -> Generator (Task y ThreadID)
threadedTask generator =
  map spawn generator


sequence : Generator (List (Task error value)) -> Generator (Task error (List value))
sequence generator =
  map Task.sequence generator
