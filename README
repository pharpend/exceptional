exceptional
===========

This is a Haskell library that exists to house a type:

```haskell
data Exceptional x
  = Failure String
  | Success x
```

It's much like `Maybe`, except instead of `Nothing`, we have `Failure
String`.

A comparison could also be made to `Either String`. I made this
library because I was dissatisfied with the `Monad` instance for
`Either`. In this type, `fail = Failure`. It's rather simple.

Installing
----------

    cabal install exceptional

Or, add `exceptional` to the `build-depends` field in your `.cabal`
file.