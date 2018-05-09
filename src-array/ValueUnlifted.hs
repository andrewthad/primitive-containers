{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}

module ValueUnlifted where

import Prelude hiding (map)

import Control.Monad.ST (ST)
import Data.Kind (Constraint)
import Data.Primitive.Types
import Data.Primitive.UnliftedArray
import qualified Data.Foldable as F

type MArr = MutableUnliftedArray
type Arr = UnliftedArray
type family Ctx a :: Constraint where
  Ctx a = PrimUnlifted a

new :: Ctx a => Int -> ST s (MutableUnliftedArray s a)
new n = unsafeNewUnliftedArray n

index :: Ctx a => UnliftedArray a -> Int -> a
index = indexUnliftedArray

read :: Ctx a => MutableUnliftedArray s a -> Int -> ST s a
read = readUnliftedArray

write :: Ctx a => MutableUnliftedArray s a -> Int -> a -> ST s ()
write = writeUnliftedArray

resize :: Ctx a => MutableUnliftedArray s a -> Int -> ST s (MutableUnliftedArray s a)
resize !src !sz = do
  dst <- unsafeNewUnliftedArray sz
  copyMutableUnliftedArray dst 0 src 0 (min sz (sizeofMutableUnliftedArray src))
  return dst

cloneMut :: Ctx a => MutableUnliftedArray s a -> Int -> Int -> ST s (MutableUnliftedArray s a)
cloneMut = cloneMutableUnliftedArray

copy :: Ctx a => MutableUnliftedArray s a -> Int -> UnliftedArray a -> Int -> Int -> ST s ()
copy = copyUnliftedArray

unsafeFreeze :: MutableUnliftedArray s a -> ST s (UnliftedArray a)
unsafeFreeze = unsafeFreezeUnliftedArray

size :: Ctx a => UnliftedArray a -> Int
size = sizeofUnliftedArray

foldr :: Ctx a => (a -> b -> b) -> b -> UnliftedArray a -> b
foldr = foldrUnliftedArray

map :: (Ctx a, Ctx b) => (a -> b) -> UnliftedArray a -> UnliftedArray b
map = mapUnliftedArray


