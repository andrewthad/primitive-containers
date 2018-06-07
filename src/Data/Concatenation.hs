{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Concatenation
  ( concatSized
  ) where

import qualified Data.List as L

concatSized :: forall m.
     (m -> Int) -- size function 
  -> m
  -> (m -> m -> m)
  -> [m]
  -> m
concatSized size empty combine = go [] where
  go :: [m] -> [m] -> m
  go !stack [] = L.foldl' combine empty (L.reverse stack)
  go !stack (x : xs) = if size x > 0
    then go (pushStack x stack) xs
    else go stack xs
  pushStack :: m -> [m] -> [m]
  pushStack x [] = [x]
  pushStack x (s : ss) = if size x >= size s
    then pushStack (combine s x) ss
    else x : s : ss

