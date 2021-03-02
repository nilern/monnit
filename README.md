# Monnit

[![Build Status](https://img.shields.io/github/workflow/status/nilern/monnit/Run%20tests.svg)](https://github.com/nilern/monnit/actions)

Monads, functors etc. for Clojure.

## Market Differentiation

* Less temporary allocations:
    - State, Reader etc. implement protocols directly instead of just wrapping
      fns to do the actual work.
    - Instead of applicatives and currying, `fmap` is variadic and has several
      fixed arities as well.
* No dynamic "monad context" Vars.

