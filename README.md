# elm-community/random-extra
This library includes lots of extra helper functions for the Random module. It makes generating pseudo-random values much easier.

If you want to add one or find a bug, please [open an issue](https://github.com/elm-community/random-extra/issues/new).

## Changelog
### 3.0.0
* Update for Elm 0.19, including switching to `elm/random`.
* Remove `Color` module.
* Rename `Random.Date.day` to `Random.Date.weekday`.
* Remove many trivial functions in `Random.Date`.
* Add `Random.Extra.bool`.
* Remove `Random.Extra.constant`; it's present in the official library.
* Change the signatures of `Random.Extra.choices` and `Random.Extra.frequency` to require an element and a list, avoid the issue of an empty list.


### 2.0.0
* Update dependencies for Elm 0.18.
* Remove `flatMap` as core's `andThen` is identical.
* Rename `flatMapN` to `andThenN`, for similar reasons.
* Rename `together` to `combine`; see #1.
* Change signature of `Random.Extra.Float.normal`; see #2.
* Add `Random.Extra.List` module; see #4.
