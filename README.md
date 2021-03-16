# elm-community/random-extra
This library includes lots of extra helper functions for the Random module. It makes generating pseudo-random values much easier.

If you want to add one or find a bug, please [open an issue](https://github.com/elm-community/random-extra/issues/new). If you want to contribute, you can open a PR adding tests.

## Changelog
### 3.2.0
* Add `Random.List.choices`
* Use a uniform shuffle for both `Random.List.shuffle` and `Random.Array.shuffle`.

### 3.1.0
* Add `sequence` and `traverse` functions
* Improve the performance of `Random.List.shuffle` and `Random.Array.shuffle`, especially on long lists/arrays.

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
