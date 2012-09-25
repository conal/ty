{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, PatternGuards #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Ty
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Typed typerefs
----------------------------------------------------------------------

module Data.Ty
  ( Ty,tyRep,ty,tyOf,tyOf1,tyOf2
  , valTyEq, valTyEq1, valTyEq2
  , Typeable,module Data.IsTy
  ) where


import Data.Typeable (Typeable,TypeRep,typeOf)
import Unsafe.Coerce (unsafeCoerce)

import Data.Proof.EQ ((:=:)(..))

import Data.IsTy

-- | Phantom type wrapper around a 'TypeRep'
data Ty a = Ty { tyRep :: TypeRep } deriving Eq

instance Show (Ty a) where show = show . tyRep

instance IsTy Ty where
  Ty a `tyEq` Ty b | a == b    = unsafeCoerce (Just Refl)
                   | otherwise = Nothing

ty :: Typeable a => Ty a
ty = tyOf (undefined :: a)

-- | The 'Ty' of a value
tyOf :: Typeable a => a -> Ty a
tyOf a = Ty (typeOf a)

-- | The 'Ty' of a value from a constructor application
tyOf1 :: forall f a. Typeable a => f a -> Ty a
tyOf1 _ = Ty (typeOf (undefined :: a))

-- | The 'Ty' of a value from a nested constructor application
tyOf2 :: forall g f a. Typeable a => g (f a) -> Ty a
tyOf2 _ = Ty (typeOf (undefined :: a))

-- | Equality of typed values. @'Just' 'Refl'@ means the the types match and the
-- values match.
valTyEq :: forall a b. (Typeable a, Typeable b, Eq a) =>
           a -> b -> Maybe (a :=: b)
oa `valTyEq` ob
  | Just Refl <- tyOf oa `tyEq` tyOf ob, oa == ob = Just Refl
  | otherwise                                     = Nothing

-- | Equality of wrapped typed values. @'Just' 'Refl'@ means the the types match
-- and the values match.
valTyEq1 :: forall f a b. (Typeable a, Typeable b, Eq (f a)) =>
            f a -> f b -> Maybe (a :=: b)
oa `valTyEq1` ob
  | Just Refl <- tyOf1 oa `tyEq` tyOf1 ob, oa == ob = Just Refl
  | otherwise                                       = Nothing

-- | Equality of doubly wrapped typed values. @'Just' 'Refl'@ means the the
-- types match and the values match.
valTyEq2 :: forall g f a b. (Typeable a, Typeable b, Eq (g (f a))) =>
            g (f a) -> g (f b) -> Maybe (a :=: b)
oa `valTyEq2` ob
  | Just Refl <- tyOf2 oa `tyEq` tyOf2 ob, oa == ob = Just Refl
  | otherwise                                       = Nothing
