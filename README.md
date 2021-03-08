# Monnit

[![Clojars Project](https://img.shields.io/clojars/v/com.deepbeginnings/monnit.svg)](https://clojars.org/com.deepbeginnings/monnit)
[![cljdoc badge](https://cljdoc.org/badge/com.deepbeginnings/monnit)](https://cljdoc.org/d/com.deepbeginnings/monnit/CURRENT)
[![Build Status](https://img.shields.io/github/workflow/status/nilern/monnit/Run%20tests.svg)](https://github.com/nilern/monnit/actions)

Monads, functors etc. for Clojure.

## Rationale (Why Yet Another Monads Library?)

I was writing various other libraries (yet to be released) and most of the time
they ended up needing efficient Functor and Monad protocols. Having copies of
those protocols in every library is clearly silly. I did not want the extra
indirection and magic of Cats (currying because of Applicative Functors,
MonadContexts in dynamic Vars, monads that just wrap fns) or the loose typing
of algo.monads (macros instead of protocols, the State monad is just a raw fn
instead of having a dedicated type).

So Monnit was born. Of course there are more monad libraries but they seem to
have a different focus (Fluokitten), be abandoned and to be honest, NIH.

## Features

* Functor, Monad etc. protocols and conveniences (e.g. a `do`-notation-like
  `mlet` macro) in [monnit.core](https://cljdoc.org/d/com.deepbeginnings/monnit/CURRENT/api/monnit.core)
* [monnit.option](https://cljdoc.org/d/com.deepbeginnings/monnit/CURRENT/api/monnit.option)
  an alternative to `nil` and `NullPointerException`s
* [monnit.result](https://cljdoc.org/d/com.deepbeginnings/monnit/CURRENT/api/monnit.result)
  an alternative to `throw` and uncaught exceptions
* [monnit.reader](https://cljdoc.org/d/com.deepbeginnings/monnit/CURRENT/api/monnit.reader)
  hide the plumbing of an extra `ctx` parameter; also an alternative to dynamic Vars
* [monnit.state](https://cljdoc.org/d/com.deepbeginnings/monnit/CURRENT/api/monnit.state)
  hide the plumbing of an extra immutable accumulator/state value
* [monnit.identity](https://cljdoc.org/d/com.deepbeginnings/monnit/CURRENT/api/monnit.identity)
  the trivial Functor and Monad; occasionally useful, just like the identity function
* [monnit.pair](https://cljdoc.org/d/com.deepbeginnings/monnit/CURRENT/api/monnit.pair)
  a two-element tuple for the State monad and general use; less indirection than a PersistentVector

## Performance

A little State monad benchmark (in `/bench`) seems to support my performance
intuitions:

```
$ clj -A:bench
Clojure 1.10.3

> (require '[monnit.benchmarks :as b]reload)
nil

> (b/benchmark-labeling 10)

# clojure.core
Evaluation count : 143400 in 60 samples of 2390 calls.
             Execution time mean : 418,939803 µs
    Execution time std-deviation : 645,276885 ns
   Execution time lower quantile : 417,622749 µs ( 2,5%)
   Execution time upper quantile : 419,771713 µs (97,5%)
                   Overhead used : 2,266132 ns

Found 1 outliers in 60 samples (1,6667 %)
	low-severe	 1 (1,6667 %)
 Variance from outliers : 1,6389 % Variance is slightly inflated by outliers

# monnit.state
Evaluation count : 153600 in 60 samples of 2560 calls.
             Execution time mean : 391,324179 µs
    Execution time std-deviation : 967,126577 ns
   Execution time lower quantile : 390,299234 µs ( 2,5%)
   Execution time upper quantile : 393,342059 µs (97,5%)
                   Overhead used : 2,266132 ns

Found 4 outliers in 60 samples (6,6667 %)
	low-severe	 1 (1,6667 %)
	low-mild	 1 (1,6667 %)
	high-mild	 2 (3,3333 %)
 Variance from outliers : 1,6389 % Variance is slightly inflated by outliers

# clojure.algo.monads/state-m
Evaluation count : 153600 in 60 samples of 2560 calls.
             Execution time mean : 391,098502 µs
    Execution time std-deviation : 346,384313 ns
   Execution time lower quantile : 390,393749 µs ( 2,5%)
   Execution time upper quantile : 391,640304 µs (97,5%)
                   Overhead used : 2,266132 ns

# cats.monad.state
Evaluation count : 8580 in 60 samples of 143 calls.
             Execution time mean : 7,025305 ms
    Execution time std-deviation : 12,159509 µs
   Execution time lower quantile : 7,002061 ms ( 2,5%)
   Execution time upper quantile : 7,043491 ms (97,5%)
                   Overhead used : 2,266132 ns
```

Here Monnit and algo.monads are about 7% faster than the vanilla Clojure
version, probably because the latter uses PersistentVectors and destructuring.
I was pleased to see that Monnit matches the performance of algo.monads while
keeping the State monad type (types, actually) distinct like Cats.

The Cats State monad is about 17 times slower than the baseline, probably due
to the indirections mentioned earlier. To be fair, the State monad does not
seem to be much of a priority for Cats; it was even removed at some point, then
re-added.

