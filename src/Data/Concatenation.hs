{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Concatenation
  ( concatSized
  , concatSized1
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

-- | Concatenate all the values in the list in the order they
-- are given. This function attempts to perform smaller concatenations
-- together. This is good for data structures that do not take
-- advantage of sharing.
concatSized :: forall m.
     (m -> Int) -- size function 
  -> m
  -> (m -> m -> m)
  -> [m]
  -> m
concatSized size empty combine = go [] where
  go :: [m] -> [m] -> m
  go !stack [] = L.foldl' combine empty (L.reverse stack)
  go !stack (x : xs) = go (pushStack x stack) xs
  pushStack :: m -> [m] -> [m]
  pushStack x [] = [x]
  pushStack x (s : ss) = if size x >= size s
    then pushStack (combine s x) ss
    else x : s : ss

-- | This function is likely to be used for things like intersection
-- where the zero-sized element is not an identity but a zero.
concatSized1 :: forall m.
     (m -> Int) -- size function 
  -> (m -> m -> m)
  -> NonEmpty m
  -> m
concatSized1 size combine (p :| ps) = go (p :| []) ps where
  go :: NonEmpty m -> [m] -> m
  go !stack [] = safeFoldl1' combine (NE.reverse stack)
  go !stack (x : xs) = go (pushStack x stack) xs
  pushStack :: m -> NonEmpty m -> NonEmpty m
  pushStack x (s :| ss) = if size x >= size s
    then case ss of
      [] -> combine s x :| []
      r : rs -> pushStack (combine s x) (r :| rs)
    else x :| (s : ss)

safeFoldl1' :: (a -> a -> a) -> NonEmpty a -> a
safeFoldl1' f (a :| as) = L.foldl' f a as

