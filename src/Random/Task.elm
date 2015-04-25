module Random.Task where
{-|

# Generators
@docs task, error, threadedTask

# Timeout Generators
@docs timeout, rangeLengthTimeout

# Chaining Task Generators
@docs sequence, parallel, optional

# Generators that communicate with mailboxes
@docs send, broadcast

-}

import Task         exposing (Task, ThreadID, spawn, succeed, fail, sleep)
import Task.Extra   as Task
import Random       exposing (Generator, float)
import Random.Extra exposing (map, constant, flatMap)
import Time         exposing (Time)
import Signal       exposing (Address)

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


threadedTask : Generator (Task x value) -> Generator (Task y ThreadID)
threadedTask generator =
  map spawn generator


sequence : Generator (List (Task error value)) -> Generator (Task error (List value))
sequence generator =
  map Task.sequence generator


parallel : Generator (List (Task error value)) -> Generator (Task error (List ThreadID))
parallel generator =
  map Task.parallel generator

optional : Generator (List (Task x value)) -> Generator (Task y (List value))
optional generator =
  map Task.optional generator

send : Address a -> Generator a -> Generator (Task error ())
send address generator =
  map (Signal.send address) generator

broadcast : List (Address a) -> Generator a -> Generator (Task error ())
broadcast addresses generator =
  map (Task.broadcast addresses) generator
