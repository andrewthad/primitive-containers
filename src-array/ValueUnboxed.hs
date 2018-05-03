{-# LANGUAGE ConstraintKinds #-}

module ValueUnboxed where

import Control.Monad.ST (ST)
import Data.Primitive.Types
import Data.Primitive.PrimArray

type MArr = MutablePrimArray
type Arr = PrimArray
type Ctx = Prim

new :: Prim a => Int -> ST s (MutablePrimArray s a)
new = newPrimArray

index :: Prim a => PrimArray a -> Int -> a
index = indexPrimArray

read :: Prim a => MutablePrimArray s a -> Int -> ST s a
read = readPrimArray

write :: Prim a => MutablePrimArray s a -> Int -> a -> ST s ()
write = writePrimArray

resize :: Prim a => MutablePrimArray s a -> Int -> ST s (MutablePrimArray s a)
resize = resizeMutablePrimArray

copy :: Prim a => MutablePrimArray s a -> Int -> PrimArray a -> Int -> Int -> ST s ()
copy = copyPrimArray

unsafeFreeze :: MutablePrimArray s a -> ST s (PrimArray a)
unsafeFreeze = unsafeFreezePrimArray

size :: Prim a => PrimArray a -> Int
size = sizeofPrimArray

foldr :: Prim a => (a -> b -> b) -> b -> PrimArray a -> b
foldr = foldrPrimArray

