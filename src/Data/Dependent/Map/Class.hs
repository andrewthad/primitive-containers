{-# language ConstraintKinds #-}
{-# language CPP #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeInType #-}

-- I really do not like the typeclasses defined in this module.
-- With the QuantifiedConstraints extension (in GHC 8.6), we should
-- be able to get rid of this entire module. But we will want to
-- wait a while before doing that.
module Data.Dependent.Map.Class
  ( Apply(..)
  , Universally(..)
  , ApplyUniversally(..)
  ) where

import Data.Kind (Type,Constraint)
import Data.Proxy (Proxy(..))
import Data.Exists (OrdForallPoly(..),EqForallPoly(..),weakenOrdering,weakenEquality)
import Data.Primitive.Contiguous (Always)
import Data.Primitive.UnliftedArray (PrimUnlifted(..))
import GHC.Exts

newtype Apply f a = Apply (f a)

class ApplyUniversally (f :: k -> Type) (x :: Type -> Constraint) where
  applyUniversallyLifted :: forall a y. Proxy f -> Proxy x -> Proxy a -> (x (f a) => y) -> y
#if MIN_VERSION_base(4,10,0) 
  applyUniversallyUnlifted :: forall a (y :: TYPE 'UnliftedRep). Proxy f -> Proxy x -> Proxy a -> (x (f a) => y) -> y
#else
  applyUniversallyUnlifted :: forall a (y :: TYPE 'PtrRepUnlifted). Proxy f -> Proxy x -> Proxy a -> (x (f a) => y) -> y
#endif

class Universally (f :: k -> Type) (x :: Type -> Constraint) where
  universally :: Proxy f -> Proxy x -> Proxy a -> (x (Apply f a) => y) -> y

instance ApplyUniversally f PrimUnlifted => PrimUnlifted (Apply f a) where
  toArrayArray# (Apply v) = applyUniversallyUnlifted (Proxy :: Proxy f) (Proxy :: Proxy PrimUnlifted) (Proxy :: Proxy a) (toArrayArray# v)
  fromArrayArray# a = applyUniversallyLifted (Proxy :: Proxy f) (Proxy :: Proxy PrimUnlifted) (Proxy :: Proxy a) (fromArrayArray# a)

instance EqForallPoly f => Eq (Apply f a) where
  Apply x == Apply y = weakenEquality (eqForallPoly x y)

instance OrdForallPoly f => Ord (Apply f a) where
  compare (Apply x) (Apply y) = weakenOrdering (compareForallPoly x y)

instance Universally f Always where
  universally _ _ _ y = y

instance ApplyUniversally f Always where
  applyUniversallyLifted _ _ _ y = y
  applyUniversallyUnlifted _ _ _ y = y

instance ApplyUniversally f PrimUnlifted => Universally f PrimUnlifted where
  universally _ _ _ y = y

