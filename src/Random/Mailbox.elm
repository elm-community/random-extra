module Random.Mailbox where

import Signal       exposing (Mailbox, Address)
import Random       exposing (Generator)
import Random.Extra exposing (map)


mailbox : Generator a -> Generator (Mailbox a)
mailbox generator =
  map Signal.mailbox generator


address : Generator a -> Generator (Address a)
address generator =
  map .address (mailbox generator)
