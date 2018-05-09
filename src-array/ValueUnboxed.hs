{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -O2 #-}

module ValueUnboxed where

import Control.Monad.ST (ST)
import Data.Primitive.Types
import Data.Primitive.PrimArray

type MArr = MutablePrimArray
type Arr = PrimArray
type family Ctx a where
  Ctx a = Prim a

new :: Ctx a => Int -> ST s (MutablePrimArray s a)
new = newPrimArray

index :: Ctx a => PrimArray a -> Int -> a
index = indexPrimArray

read :: Ctx a => MutablePrimArray s a -> Int -> ST s a
read = readPrimArray

write :: Ctx a => MutablePrimArray s a -> Int -> a -> ST s ()
write = writePrimArray

resize :: Ctx a => MutablePrimArray s a -> Int -> ST s (MutablePrimArray s a)
resize = resizeMutablePrimArray

copy :: Ctx a => MutablePrimArray s a -> Int -> PrimArray a -> Int -> Int -> ST s ()
copy = copyPrimArray

unsafeFreeze :: MutablePrimArray s a -> ST s (PrimArray a)
unsafeFreeze = unsafeFreezePrimArray

size :: Ctx a => PrimArray a -> Int
size = sizeofPrimArray

foldr :: Ctx a => (a -> b -> b) -> b -> PrimArray a -> b
foldr = foldrPrimArray

map :: (Ctx a, Ctx b) => (a -> b) -> PrimArray a -> PrimArray b
map = mapPrimArray

