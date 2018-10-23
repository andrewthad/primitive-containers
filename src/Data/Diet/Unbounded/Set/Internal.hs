{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Diet.Unbounded.Set.Internal
  ( Set
  , empty
  , singleton
  , append
  , member
  , equals
  , showsPrec
  ) where

import Prelude hiding (showsPrec)

import Data.Primitive.Contiguous (Contiguous,Element,Mutable)

import qualified Data.Diet.Set.Internal as S
import qualified Data.Primitive.Contiguous as I

-- todo: switch to using an unboxed sum instead of
-- Maybe once GHC 8.4.3 becomes prevalent.
--
-- If the first Maybe is Just, then everything from negative
-- infinity (whatever that may mean for the type at hand) up
-- to the value is included in the set. It works similarly
-- for the second Maybe and positive infinity. Internally,
-- we must uphold the invariant that the range up from negative
-- infinity and the one up to positive infinity do not overlap
-- with the diet set in the middle and that they are not
-- adjacent to it (according to the Enum instance).
--
-- The second data constructor, SetAll, means that all values
-- of type @a@ are included in the Set. We do actually need
-- a separate data constructor for this since there is no
-- way to communicate it with the first one.
data Set arr a
  = SetSome !(Maybe a) !(S.Set arr a) !(Maybe a)
  | SetAll

empty :: Contiguous arr => Set arr a
empty = SetSome Nothing S.empty Nothing

equals :: (Contiguous arr, Element arr a, Eq a) => Set arr a -> Set arr a -> Bool
equals SetAll SetAll = True
equals SetAll (SetSome _ _ _) = False
equals (SetSome _ _ _) SetAll = False
equals (SetSome a b c) (SetSome x y z) = a == x && c == z && S.equals b y

singleton :: (Contiguous arr, Element arr a, Ord a)
  => Maybe a -- ^ lower inclusive bound, @Nothing@ means @-∞@
  -> Maybe a -- ^ upper inclusive bound, @Nothing@ means @+∞@
  -> Set arr a
singleton Nothing Nothing = SetAll
singleton Nothing (Just hi) = SetSome (Just hi) S.empty Nothing
singleton (Just lo) Nothing = SetSome Nothing S.empty (Just lo)
singleton (Just lo) (Just hi) = SetSome Nothing (S.singleton lo hi) Nothing

append :: forall arr a. (Contiguous arr, Element arr a, Ord a, Enum a)
  => Set arr a
  -> Set arr a
  -> Set arr a
append SetAll _ = SetAll
append (SetSome _ _ _) SetAll = SetAll
append (SetSome Nothing a Nothing) (SetSome Nothing b Nothing) =
  SetSome Nothing (S.append a b) Nothing
append (SetSome (Just infHiA) a Nothing) (SetSome Nothing b Nothing) =
  let (infHi, trimmedB) = establishInfinityHi infHiA b
   in SetSome (Just infHi) (S.append a trimmedB) Nothing
append (SetSome Nothing a Nothing) (SetSome (Just infHiB) b Nothing) =
  let (infHi, trimmedA) = establishInfinityHi infHiB a
   in SetSome (Just infHi) (S.append trimmedA b) Nothing
append (SetSome (Just infHiA) a Nothing) (SetSome (Just infHiB) b Nothing) =
  case compare infHiA infHiB of
    EQ -> SetSome (Just infHiA) (S.append a b) Nothing
    LT -> 
      let (infHi, trimmedA) = establishInfinityHi infHiB a
       in SetSome (Just infHi) (S.append trimmedA b) Nothing
    GT -> 
      let (infHi, trimmedB) = establishInfinityHi infHiA b
       in SetSome (Just infHi) (S.append a trimmedB) Nothing
append (SetSome Nothing a (Just infLoA)) (SetSome Nothing b Nothing) =
  let (infLo, trimmedB) = establishInfinityLo infLoA b
   in SetSome Nothing (S.append a trimmedB) (Just infLo)
append (SetSome Nothing a Nothing) (SetSome Nothing b (Just infLoB)) =
  let (infLo, trimmedA) = establishInfinityLo infLoB a
   in SetSome Nothing (S.append trimmedA b) (Just infLo)
append (SetSome Nothing a (Just infLoA)) (SetSome Nothing b (Just infLoB)) =
  case compare infLoA infLoB of
    EQ -> SetSome Nothing (S.append a b) (Just infLoB)
    LT -> 
      let (infLo, trimmedB) = establishInfinityLo infLoA b
       in SetSome Nothing (S.append a trimmedB) (Just infLo)
    GT -> 
      let (infLo, trimmedA) = establishInfinityLo infLoB a
       in SetSome Nothing (S.append trimmedA b) (Just infLo)
append (SetSome (Just infHiA) a (Just infLoA)) (SetSome Nothing b Nothing) =
  case establishInfinityBoth infHiA infLoA b of
    Nothing -> SetAll
    Just (infHi,infLo,trimmedB) -> SetSome (Just infHi) (S.append a trimmedB) (Just infLo)
append (SetSome Nothing a Nothing) (SetSome (Just infHiB) b (Just infLoB)) =
  case establishInfinityBoth infHiB infLoB a of
    Nothing -> SetAll
    Just (infHi,infLo,trimmedA) -> SetSome (Just infHi) (S.append trimmedA b) (Just infLo)
append (SetSome (Just infHiA) a (Just infLoA)) (SetSome (Just infHiB) b (Just infLoB)) =
  generalAppend (max infHiA infHiB) (min infLoA infLoB) a b
append (SetSome Nothing a (Just infLoA)) (SetSome (Just infHiB) b (Just infLoB)) =
  generalAppend infHiB (min infLoA infLoB) a b
append (SetSome (Just infHiA) a (Just infLoA)) (SetSome Nothing b (Just infLoB)) =
  generalAppend infHiA (min infLoA infLoB) a b
append (SetSome (Just infHiA) a Nothing) (SetSome (Just infHiB) b (Just infLoB)) =
  generalAppend (max infHiA infHiB) infLoB a b
append (SetSome (Just infHiA) a (Just infLoA)) (SetSome (Just infHiB) b Nothing) =
  generalAppend (max infHiA infHiB) infLoA a b
append (SetSome Nothing a (Just infLoA)) (SetSome (Just infHiB) b Nothing) =
  generalAppend infHiB infLoA a b
append (SetSome (Just infHiA) a Nothing) (SetSome Nothing b (Just infLoB)) =
  generalAppend infHiA infLoB a b

generalAppend :: (Contiguous arr, Ord a, Enum a, Element arr a)
  => a -> a -> S.Set arr a -> S.Set arr a -> Set arr a
generalAppend infHiX infLoX a b =
  case establishInfinityBoth infHiX infLoX (S.append a b) of
    Nothing -> SetAll
    Just (infHi,infLo,trimmed) -> SetSome (Just infHi) trimmed (Just infLo)

-- This takes an value @a@ which is the upper bound of (-∞,a] range.
-- It also takes a diet set. It removes everything from the set
-- that is contained by the up-from-negative-infinity range, and
-- it also removes a range adjacent to @a@. If a range adjacent to
-- @a@ was removed, then the returned value will be the upper bound
-- of the removed adjacent range.
establishInfinityHi :: forall arr a. (Contiguous arr, Element arr a, Ord a, Enum a)
  => a -- upper bound from negative infinity
  -> S.Set arr a -- diet set
  -> (a, S.Set arr a) -- new upper bound, trimmed diet set
establishInfinityHi a s = case locateAdjacentAbove a s of
  Right ix ->
    let upper = S.indexUpper ix s
     in (upper,S.slice (ix + 1) (S.size s - 1) s)
  Left ix -> (a,S.slice ix (S.size s - 1) s)

establishInfinityLo :: forall arr a. (Contiguous arr, Element arr a, Ord a, Enum a)
  => a -- lower bound from positive infinity
  -> S.Set arr a -- diet set
  -> (a, S.Set arr a) -- new lower bound, trimmed diet set
establishInfinityLo a s = case locateAdjacentBelow a s of
  Right ix ->
    let lower = S.indexLower ix s
     in (lower,S.slice 0 (ix - 1) s)
  Left ix -> (a, S.slice 0 ix s)

-- this is a tweaked version of locate. If the element
-- isn't found in the diet set, it looks at its predecessor
-- to see if it is present so that we can collapse a maximal
-- number of ranges. Left gives the index of the range to
-- the left of (meaning: less than) the element.
--
-- Right: [0,n-1]
-- Left: [-1,n-1]
locateAdjacentBelow :: forall arr a. (Contiguous arr, Element arr a, Ord a, Enum a)
  => a -- lower bound from positive infinity
  -> S.Set arr a -- diet set
  -> Either Int Int
locateAdjacentBelow a s = case S.locate a s of
  Right ix -> Right ix
  Left ix -> if ix == 0
    then Left (-1)
    else if S.indexUpper (ix - 1) s == pred a
      then Right (ix - 1)
      else Left (ix - 1)

-- this is a tweaked version of locate. If the element
-- isn't found in the diet set, it looks at its successor
-- to see if it is present so that we can collapse a maximal
-- number of ranges. Left gives the index of the range to
-- the right of (meaning: greater than) the element.
--
-- Right: [0,n-1]
-- Left: [0,n]
locateAdjacentAbove :: forall arr a. (Contiguous arr, Element arr a, Ord a, Enum a)
  => a -- upper bound from negative infinity
  -> S.Set arr a -- diet set
  -> Either Int Int
locateAdjacentAbove a s = case S.locate a s of
  Right ix -> Right ix
  Left ix -> if ix == S.size s
    then Left ix
    else if S.indexLower ix s == succ a
      then Right ix
      else Left ix

establishInfinityBoth :: forall arr a. (Contiguous arr, Element arr a, Ord a, Enum a)
  => a -- upper bound from negative infinity
  -> a -- lower bound from positive infinity
  -> S.Set arr a -- diet set
  -> Maybe (a, a, S.Set arr a) -- new upper bound, new lower bound, trimmed diet set
establishInfinityBoth negInfHi posInfLo s = if posInfLo <= negInfHi
  then Nothing
  else case locateAdjacentAbove negInfHi s of
    Left loIx -> case locateAdjacentBelow posInfLo s of
      Left hiIx -> Just (negInfHi,posInfLo,S.slice loIx hiIx s)
      Right hiIx -> Just (negInfHi,S.indexLower hiIx s,S.slice loIx (hiIx - 1) s)
    Right loIx -> case locateAdjacentBelow posInfLo s of
      Left hiIx -> Just (S.indexUpper loIx s,posInfLo,S.slice (loIx + 1) hiIx s)
      Right hiIx -> if hiIx <= loIx
        then Nothing
        else Just (S.indexUpper loIx s, S.indexLower hiIx s, S.slice (loIx + 1) (hiIx - 1) s)
  
member :: forall arr a. (Contiguous arr, Element arr a, Ord a)
  => a
  -> Set arr a
  -> Bool
member _ SetAll = True
member x (SetSome negInfHi s posInfLo) =
     maybe False (\hi -> hi >= x) negInfHi
  || maybe False (\lo -> lo <= x) posInfLo
  || S.member x s
{-# INLINEABLE member #-}

showsPrec :: (Contiguous arr, Element arr a, Show a)
  => Int
  -> Set arr a
  -> ShowS
showsPrec _ SetAll = showString "[(-∞,+∞)]"
showsPrec p (SetSome negInfHi s posInfLo) = showParen (p > 10) $
  showString "fromList " . showListInf shows negInfHi (S.toList s) posInfLo

showListInf :: (a -> ShowS) -> Maybe a -> [(a,a)] -> Maybe a -> ShowS
showListInf showx mnegInfHi [] mposInfLo s = case mnegInfHi of
  Nothing -> case mposInfLo of
    Nothing -> "[]" ++ s
    Just posInfLo -> '[' : showPosInfLo showx posInfLo (']' : s)
  Just negInfHi -> case mposInfLo of
    Nothing -> '[' : showNegInfHi showx negInfHi (']' : s)
    Just posInfLo -> '[' : showNegInfHi showx negInfHi (',' : showPosInfLo showx posInfLo (']' : s))
showListInf showx mnegInfHi ((a0,b0):xs) mposInfLo s =
  '[' : maybe id (\negInfHi s' -> showNegInfHi showx negInfHi (',' : s')) mnegInfHi ('(' : showx a0 (',' : showx b0 (')' : showl xs)))
  where
    showl [] = maybe id (\posInfLo -> showChar ',' . showPosInfLo showx posInfLo) mposInfLo (']' : s)
    showl ((a,b):ys) = ',' : '(' : showx a (',' : showx b (')' : showl ys))

showNegInfHi :: (a -> ShowS) -> a -> ShowS
showNegInfHi showx x s = "(-∞," ++ showx x (")" ++ s)

showPosInfLo :: (a -> ShowS) -> a -> ShowS
showPosInfLo showx x s = '(' : (showx x (",+∞)" ++ s))

