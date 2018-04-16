# primitive-containers

This package provides types similar to those found in containers.
However, they are backed by contiguous arrays instead of trees.
These should perform better for lookups but worse for any kind of
update.

## Interfaces

These nearly-identical module interfaces are accomplished
by internal use of backpack, which allows us to parameterize
over a constraint.

### Data.Set.Unboxed

    data Set a
    instance (Eq a, Prim a) => Eq (Set a)
    instance (Ord a, Prim a) => Ord (Set a)
    instance (Show a, Prim a) => Show (Set a)
    instance Prim a => Semigroup (Set a)
    instance Prim a => Monoid (Set a)
    lookup :: Prim a => Set a -> Maybe a
    singleton :: Prim a => a -> Set a

### Data.Set.Unlifted

    data Set a
    instance (Eq a, PrimUnlifted a) => Eq (Set a)
    instance (Ord a, PrimUnlifted a) => Ord (Set a)
    instance (Show a, PrimUnlifted a) => Show (Set a)
    instance PrimUnlifted a => Semigroup (Set a)
    instance PrimUnlifted a => Monoid (Set a)
    lookup :: PrimUnlifted a => Set a -> Maybe a
    singleton :: PrimUnlifted a => a -> Set a

### Data.Set.Unlifted

    data Set a
    instance Eq a => Eq (Set a)
    instance Ord a => Ord (Set a)
    instance Show a => Show (Set a)
    instance Semigroup (Set a)
    instance Monoid (Set a)
    lookup :: Set a -> Maybe a
    singleton :: a -> Set a

