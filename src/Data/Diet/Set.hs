{-|

The modules in this hierarchy implement sets of nonoverlapping,
nonadjacent intervals. In the literature, one such implementation of
these is known as
<http://web.engr.oregonstate.edu/~erwig/diet/ Discrete Interval Encoding Trees>
(DIETs). This implementation is discussed in
<http://web.engr.oregonstate.edu/~erwig/papers/Diet_JFP98.pdf Diets for Fat Sets>,
Martin Erwig. Journal of Functional Programming, Vol. 8, No. 6, 627-632, 1998.
In this package, we use the term diet set to refer to not just that one
implementation but to any set of nonoverlapping, nonadjacent intervals.

These are not the same as interval sets. An interval set preserves
the original intervals that the user inserted into the set. A diet set
will coalesce adjacent or overlapping ranges. For example:

>>> ⦃[2,6]⦄ ⋄ ⦃[1,3]⦄ ⋄ ⦃[8,11]⦄ ⋄ ⦃[12,12]⦄ 
⦃[1,6],[8,12]⦄

The implementation in this packages is optimized for reads. Building
a diet set is expensive since the array-backed implementation cannot
do any sharing when it creates a new data structure. However, testing
for membership is @O(log n)@. 

-}

module Data.Diet.Set () where
