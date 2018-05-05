{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-- The simplifiable class constraints warning is suppressed because
-- these redundant Always constraints are needed to make backpack 
-- see that type signatures match up.
{-# OPTIONS_GHC -O2 -fno-warn-simplifiable-class-constraints #-}

module ValueUnlifted where

import Prelude hiding (map)

import Control.Monad.ST (ST)
import Data.Primitive.Types
import Data.Primitive.UnliftedArray
import qualified Data.Foldable as F

type MArr = MutableUnliftedArray
type Arr = UnliftedArray
type Ctx = PrimUnlifted

new :: PrimUnlifted a => Int -> ST s (MutableUnliftedArray s a)
new n = unsafeNewUnliftedArray n

index :: PrimUnlifted a => UnliftedArray a -> Int -> a
index = indexUnliftedArray

read :: PrimUnlifted a => MutableUnliftedArray s a -> Int -> ST s a
read = readUnliftedArray

write :: PrimUnlifted a => MutableUnliftedArray s a -> Int -> a -> ST s ()
write = writeUnliftedArray

resize :: PrimUnlifted a => MutableUnliftedArray s a -> Int -> ST s (MutableUnliftedArray s a)
resize !src !sz = do
  dst <- unsafeNewUnliftedArray sz
  copyMutableUnliftedArray dst 0 src 0 (min sz (sizeofMutableUnliftedArray src))
  return dst

cloneMut :: PrimUnlifted a => MutableUnliftedArray s a -> Int -> Int -> ST s (MutableUnliftedArray s a)
cloneMut = cloneMutableUnliftedArray

copy :: PrimUnlifted a => MutableUnliftedArray s a -> Int -> UnliftedArray a -> Int -> Int -> ST s ()
copy = copyUnliftedArray

unsafeFreeze :: MutableUnliftedArray s a -> ST s (UnliftedArray a)
unsafeFreeze = unsafeFreezeUnliftedArray

size :: PrimUnlifted a => UnliftedArray a -> Int
size = sizeofUnliftedArray

foldr :: PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
foldr = F.foldr

map :: (PrimUnlifted a, PrimUnlifted b) => (a -> b) -> UnliftedArray a -> UnliftedArray b
map = fmap


