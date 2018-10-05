{-| 

This module only exists for documentation. It should never be imported.

The interval maps provided by the submodules of `Data.Map.Interval`
coallesce overlapping intervals. Their behavior differs from that
of the type from the `IntervalMap` package. The interval map from
that package preserves all the original interval that were used
as keys for the map. The interval map from this package creates a
new interval from the overlap, combining the values.

There are several points in the design space to explore with this
kind of interval map. A motivation for some of these variants is
having `Eq` instances that satisfy a bidirectional variant of the
substition law. That is:

> ∀ x y. (x == y ↔ ∀ f. f x == f y)

Here are the different design choices that we face:

* Discrete (D) vs Continuous (C): The basically comes down to whether or
  not there is an `Enum` instance for the type. Although it cannot be
  enforced by the type system, continuous-keyed maps should not use discrete
  types as keys. The bidirectional substituion law is not upheld in this
  case. The discrete-keyed interval map uses `succ` and `pred`
  to coalesce adjacent intervals. The continuous-keyed interval map,
  assuming that unequal values have infinitely many values between
  them, only considers merging adjacent intervals when an open interval
  butts up against a closed interval with a matching key.
* Bounded (B) vs Unbounded (U): Is there a Bounded instance for the type?
  Bounded types can treat `maxBound` as infinity. Unbounded types like
  `Integer` and `Text` have no value for infinity. If the key type has
  a `Bounded` instance, it is incorrect to use it in an unbounded interval
  map since the `Eq` instance will not satisfy the bidirectional substitution law.
* Partial (P) vs Total (T): Is there a value corresponding to every key?
  The decides whether or not the return value of `lookup` is wrapped in a
  `Maybe`. Total maps with unconstrained values also have an `Applicative`
  instance. The internal representation of total maps is also more
  efficient than that of partial maps since we only need to store the
  upper bound of each interval.
* Strict (S) vs Lazy (L): The names here a little here are a little
  misleading. The strict variant uses on an `Eq` instance for values
  to coallesce adjacent ranges. For example, with discrete integers,
  the interval-value pairs ([4,6],12) and ([7,9],12) can be coallesced
  because 6 is adjacent to 7 and both pairs share value 12. Coalescing
  in this way is crucial to satisfying the bidirectional substitution
  law. It also induces value-strictness. Some users may prefer
  laziness in the values. This is also offered, but none of the
  value-lazy interval maps have `Eq` instances since it is not possible
  to satisfy the bidirectional substitution law without forcing the
  values.

The modules are named using acronyms that refer to various combinations
of these flavors. For exmaple, `Data.Map.Interval.DUTS` provides the
discrete unbounded total strict interval map. Some combinations are not
provided because the author is unaware of useful types that meet the
restrictions (for example, pairing continuous and bounded seems
dubious).

For users who want to use 'Double' as the key type, it is recommended
that CUxx be used since the `Enum` instance for `Double` is dubious.

-}
module Data.Map.Interval () where
