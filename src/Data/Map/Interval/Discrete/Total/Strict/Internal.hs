module Data.Map.Interval.Discrete.Total.Strict.Internal
  ( Map
  , pure
  , lookup
  ) where

-- | The key array is the same length as the value array. Every key
--   is the upper bound of a range. The keys array always has a length
--   of at least one. The last element is always maxBound. The lowest bound
--   is assumed to be minBound. For example, the interval map of Int16:
--
--     [-inf,5],[6,17],[18,20],[21,+inf]
--
--   Would be represented by the keys:
--   
--     5,17,20,65536
data Map karr varr k v = Map !(karr k) !(varr v)

pure :: (Contiguous karr, Contiguous varr, Bounded k) => v -> Map karr varr k v
pure v = Map
  (runST $ do
     !(arr :: Mutable karr s k) <- I.new 1
     I.write arr 0 maxBound
     I.unsafeFreeze arr
  )
  (runST $ do
     !(arr :: Mutable varr s v) <- I.new 1
     I.write arr 0 v
     I.unsafeFreeze arr
  )

lookup :: forall karr varr k v. (Contiguous karr, Element karr k, Ord k, Contiguous varr, Element varr v) => k -> Map karr varr k v -> v
lookup a (Map keys vals) = go 0 (I.size vals - 1) where
  go :: Int -> Int -> v
  go !start !end = if end == start
    then
      let !(# v #) = I.index# vals start
       in v
    else
      let !mid = div (end + start) 2
          !valHi = I.index keys mid
       in case P.compare a valHi of
            LT -> go start mid
            EQ -> case I.index# vals mid of
              (# v #) -> Just v
            GT -> go (mid + 1) end
{-# INLINEABLE lookup #-}


