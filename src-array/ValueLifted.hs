{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}

module ValueLifted where

import Prelude hiding (map)

import Control.Monad.ST (ST)
import Data.Kind
import Data.Primitive.Types
import Data.Primitive.Array
import qualified Data.Foldable as F

type MArr = MutableArray
type Arr = Array
type family Ctx a :: Constraint where
  Ctx a = ()

errorThunk :: a
errorThunk = error "primitive-containers: ValueLifted: uninitialized element"
{-# NOINLINE errorThunk #-}

new :: Ctx a => Int -> ST s (MutableArray s a)
new n = newArray n errorThunk

index :: Ctx a => Array a -> Int -> a
index = indexArray

read :: Ctx a => MutableArray s a -> Int -> ST s a
read = readArray

write :: Ctx a => MutableArray s a -> Int -> a -> ST s ()
write = writeArray

resize :: Ctx a => MutableArray s a -> Int -> ST s (MutableArray s a)
resize !src !sz = do
  dst <- newArray sz errorThunk
  copyMutableArray dst 0 src 0 (min sz (sizeofMutableArray src))
  return dst

cloneMut :: MutableArray s a -> Int -> Int -> ST s (MutableArray s a)
cloneMut = cloneMutableArray

copy :: Ctx a => MutableArray s a -> Int -> Array a -> Int -> Int -> ST s ()
copy = copyArray

unsafeFreeze :: MutableArray s a -> ST s (Array a)
unsafeFreeze = unsafeFreezeArray

size :: Ctx a => Array a -> Int
size = sizeofArray

foldr ::Ctx a => (a -> b -> b) -> b -> Array a -> b
foldr = F.foldr

map :: (Ctx a, Ctx b) => (a -> b) -> Array a -> Array b
map = fmap

