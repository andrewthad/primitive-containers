{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 -Wall #-}
module Data.Diet.Set.Internal
  ( Set
  , empty
  , singleton
  , append
  , member
  , concat
  , equals
  , showsPrec
    -- list conversion
  , fromListN
  , fromList
  , toList
  ) where

import Prelude hiding (lookup,showsPrec,concat,map,foldr)

import Control.Monad.ST (ST,runST)
import Data.Internal (Contiguous,Element,Mutable)
import qualified Data.Foldable as F
import qualified Prelude as P
import qualified Data.Internal as I

-- The key array is twice as long as the value array since
-- everything is stored as a range. Also, figure out how to
-- unpack these two arguments at some point.
newtype Set arr a = Set (arr a)

empty :: Contiguous arr => Set arr a
empty = Set I.empty

equals :: (Contiguous arr, Element arr a, Eq a) => Set arr a -> Set arr a -> Bool
equals (Set x) (Set y) = I.equals x y

fromListN :: (Contiguous arr, Element arr a, Ord a, Enum a) => Int -> [(a,a)] -> Set arr a
fromListN _ xs = concat (P.map (uncurry singleton) xs)

fromList :: (Contiguous arr, Element arr a, Ord a, Enum a) => [(a,a)] -> Set arr a
fromList = fromListN 1

concat :: forall arr a. (Contiguous arr, Element arr a, Ord a, Enum a)
  => [Set arr a]
  -> Set arr a
concat = go [] where
  go :: [Set arr a] -> [Set arr a] -> Set arr a
  go !stack [] = F.foldl' append empty stack
  go !stack (x : xs) = if size x > 0
    then go (pushStack x stack) xs
    else go stack xs
  pushStack :: Set arr a -> [Set arr a] -> [Set arr a]
  pushStack x [] = [x]
  pushStack x (s : ss) = if size x >= size s
    then pushStack (append x s) ss
    else x : s : ss

singleton :: forall arr a. (Contiguous arr, Element arr a, Ord a)
  => a -- ^ lower inclusive bound
  -> a -- ^ upper inclusive bound
  -> Set arr a
singleton !lo !hi = if lo <= hi
  then Set
    ( runST $ do
        !(arr :: Mutable arr s a) <- I.new 2
        I.write arr 0 lo
        I.write arr 1 hi
        I.unsafeFreeze arr
    )
  else empty

member :: forall arr a. (Contiguous arr, Element arr a, Ord a)
  => a
  -> Set arr a
  -> Bool
member a (Set arr) = go 0 ((div (I.size arr) 2) - 1) where
  go :: Int -> Int -> Bool
  go !start !end = if end <= start
    then if end == start
      then 
        let !valLo = I.index arr (2 * start)
            !valHi = I.index arr (2 * start + 1)
         in a >= valLo && a <= valHi
      else False
    else
      let !mid = div (end + start + 1) 2
          !valLo = I.index arr (2 * mid)
       in case P.compare a valLo of
            LT -> go start (mid - 1)
            EQ -> True
            GT -> go mid end
{-# INLINEABLE member #-}

append :: forall arr a. (Contiguous arr, Element arr a, Ord a, Enum a)
  => Set arr a
  -> Set arr a
  -> Set arr a
append (Set keysA) (Set keysB)
  | szA < 1 = Set keysB
  | szB < 1 = Set keysA
  | otherwise = runST action
  where
  !szA = div (I.size keysA) 2
  !szB = div (I.size keysB) 2
  action :: forall s. ST s (Set arr a)
  action = do
    !(keysDst :: Mutable arr s a) <- I.new (max szA szB * 8)
    let writeKeyRange :: Int -> a -> a -> ST s ()
        writeKeyRange !ix !lo !hi = do
          I.write keysDst (2 * ix) lo
          I.write keysDst (2 * ix + 1) hi
        writeDstHiKey :: Int -> a -> ST s ()
        writeDstHiKey !ix !hi = I.write keysDst (2 * ix + 1) hi
        readDstHiKey :: Int -> ST s a
        readDstHiKey !ix = I.read keysDst (2 * ix + 1)
        indexLoKeyA :: Int -> a
        indexLoKeyA !ix = I.index keysA (ix * 2)
        indexLoKeyB :: Int -> a
        indexLoKeyB !ix = I.index keysB (ix * 2)
        indexHiKeyA :: Int -> a
        indexHiKeyA !ix = I.index keysA (ix * 2 + 1)
        indexHiKeyB :: Int -> a
        indexHiKeyB !ix = I.index keysB (ix * 2 + 1)
    -- In the go functon, ixDst is always at least one. Similarly,
    -- all key arguments are always greater than minBound.
    let go :: Int -> a -> a -> Int -> a -> a -> Int -> ST s Int
        go !ixA !loA !hiA !ixB !loB !hiB !ixDst = do
          prevHi <- readDstHiKey (ixDst - 1) 
          case compare loA loB of
            LT -> do
              let (upper,ixA') = if hiA < loB
                    then (hiA,ixA + 1)
                    else (pred loB,ixA)
              ixDst' <- if pred loA == prevHi
                then do
                  writeDstHiKey (ixDst - 1) upper
                  return ixDst
                else do
                  writeKeyRange ixDst loA upper
                  return (ixDst + 1)
              if ixA' < szA
                then do
                  let (loA',hiA') = if hiA < loB
                        then (indexLoKeyA ixA',indexHiKeyA ixA')
                        else (loB,hiA)
                  go ixA' loA' hiA' ixB loB hiB ixDst'
                else copyB ixB loB hiB ixDst'
            GT -> do
              let (upper,ixB') = if hiB < loA
                    then (hiB,ixB + 1)
                    else (pred loA,ixB)
              ixDst' <- if pred loB == prevHi
                then do
                  writeDstHiKey (ixDst - 1) upper
                  return ixDst
                else do
                  writeKeyRange ixDst loB upper
                  return (ixDst + 1)
              if ixB' < szB
                then do
                  let (loB',hiB') = if hiB < loA
                        then (indexLoKeyB ixB',indexHiKeyB ixB')
                        else (loA,hiB)
                  go ixA loA hiA ixB' loB' hiB' ixDst'
                else copyA ixA loA hiA ixDst'
            EQ -> do
              case compare hiA hiB of
                LT -> do
                  ixDst' <- if pred loA == prevHi
                    then do
                      writeDstHiKey (ixDst - 1) hiA
                      return ixDst
                    else do
                      writeKeyRange ixDst loA hiA
                      return (ixDst + 1)
                  let ixA' = ixA + 1
                      loB' = succ hiA
                  if ixA' < szA
                    then go ixA' (indexLoKeyA ixA') (indexHiKeyA ixA') ixB loB' hiB ixDst'
                    else copyB ixB loB' hiB ixDst'
                GT -> do
                  ixDst' <- if pred loB == prevHi
                    then do
                      writeDstHiKey (ixDst - 1) hiB
                      return ixDst
                    else do
                      writeKeyRange ixDst loB hiB
                      return (ixDst + 1)
                  let ixB' = ixB + 1
                      loA' = succ hiB
                  if ixB' < szB
                    then go ixA loA' hiA ixB' (indexLoKeyB ixB') (indexHiKeyB ixB') ixDst'
                    else copyA ixA loA' hiA ixDst'
                EQ -> do
                  ixDst' <- if pred loB == prevHi
                    then do
                      writeDstHiKey (ixDst - 1) hiB
                      return ixDst
                    else do
                      writeKeyRange ixDst loB hiB
                      return (ixDst + 1)
                  let ixA' = ixA + 1
                      ixB' = ixB + 1
                  if ixA' < szA
                    then if ixB' < szB
                      then go ixA' (indexLoKeyA ixA') (indexHiKeyA ixA') ixB' (indexLoKeyB ixB') (indexHiKeyB ixB') ixDst'
                      else copyA ixA' (indexLoKeyA ixA') (indexHiKeyA ixA') ixDst'
                    else if ixB' < szB
                      then copyB ixB' (indexLoKeyB ixB') (indexHiKeyB ixB') ixDst'
                      else return ixDst'
        copyB :: Int -> a -> a -> Int -> ST s Int
        copyB !ixB !loB !hiB !ixDst = do
          prevHi <- readDstHiKey (ixDst - 1) 
          ixDst' <- if pred loB == prevHi
            then do
              writeDstHiKey (ixDst - 1) hiB
              return ixDst
            else do
              writeKeyRange ixDst loB hiB
              return (ixDst + 1)
          let ixB' = ixB + 1
              remaining = szB - ixB'
          I.copy keysDst (ixDst' * 2) keysB (ixB' * 2) (remaining * 2)
          return (ixDst' + remaining)
        copyA :: Int -> a -> a -> Int -> ST s Int
        copyA !ixA !loA !hiA !ixDst = do
          prevHi <- readDstHiKey (ixDst - 1) 
          ixDst' <- if pred loA == prevHi
            then do
              writeDstHiKey (ixDst - 1) hiA
              return ixDst
            else do
              writeKeyRange ixDst loA hiA
              return (ixDst + 1)
          let ixA' = ixA + 1
              remaining = szA - ixA'
          I.copy keysDst (ixDst' * 2) keysA (ixA' * 2) (remaining * 2)
          return (ixDst' + remaining)
    let !loA0 = indexLoKeyA 0
        !loB0 = indexLoKeyB 0
        !hiA0 = indexHiKeyA 0
        !hiB0 = indexHiKeyB 0
    total <- case compare loA0 loB0 of
      LT -> if hiA0 < loB0
        then do
          writeKeyRange 0 loA0 hiA0
          if 1 < szA
            then go 1 (indexLoKeyA 1) (indexHiKeyA 1) 0 loB0 hiB0 1
            else copyB 0 loB0 hiB0 1
        else do
          -- here we know that hiA > loA
          let !upperA = pred loB0
          writeKeyRange 0 loA0 upperA
          go 0 loB0 hiA0 0 loB0 hiB0 1
      EQ -> case compare hiA0 hiB0 of
        LT -> do
          writeKeyRange 0 loA0 hiA0
          if 1 < szA
            then go 1 (indexLoKeyA 1) (indexHiKeyA 1) 0 (succ hiA0) hiB0 1
            else copyB 0 (succ hiA0) hiB0 1
        GT -> do
          writeKeyRange 0 loB0 hiB0
          if 1 < szB
            then go 0 (succ hiB0) hiA0 1 (indexLoKeyB 1) (indexHiKeyB 1) 1
            else copyA 0 (succ hiB0) hiA0 1
        EQ -> do
          writeKeyRange 0 loA0 hiA0
          if 1 < szA
            then if 1 < szB
              then go 1 (indexLoKeyA 1) (indexHiKeyA 1) 1 (indexLoKeyB 1) (indexHiKeyB 1) 1
              else copyA 1 (indexLoKeyA 1) (indexHiKeyA 1) 1
            else if 1 < szB
              then copyB 1 (indexLoKeyB 1) (indexHiKeyB 1) 1
              else return 1
      GT -> if hiB0 < loA0
        then do
          writeKeyRange 0 loB0 hiB0
          if 1 < szB
            then go 0 loA0 hiA0 1 (indexLoKeyB 1) (indexHiKeyB 1) 1
            else copyA 0 loA0 hiA0 1
        else do
          let !upperB = pred loA0
          writeKeyRange 0 loB0 upperB
          go 0 loA0 hiA0 0 loA0 hiB0 1
    !keysFinal <- I.resize keysDst (total * 2)
    fmap Set (I.unsafeFreeze keysFinal)

size :: (Contiguous arr, Element arr a) => Set arr a -> Int
size (Set arr) = I.size arr 

toList :: (Contiguous arr, Element arr a) => Set arr a -> [(a,a)]
toList = foldr (\lo hi xs -> (lo,hi) : xs) []

foldr :: (Contiguous arr, Element arr a) => (a -> a -> b -> b) -> b -> Set arr a -> b
foldr f z (Set arr) =
  let !sz = div (I.size arr) 2
      go !i
        | i == sz = z
        | otherwise =
            let !lo = I.index arr (i * 2)
                !hi = I.index arr (i * 2 + 1)
             in f lo hi (go (i + 1))
   in go 0

showsPrec :: (Contiguous arr, Element arr a, Show a)
  => Int
  -> Set arr a
  -> ShowS
showsPrec p xs = showParen (p > 10) $
  showString "fromList " . shows (toList xs)

