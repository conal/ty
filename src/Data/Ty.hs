{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
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

module Data.Ty (Ty,tyRep,ty,tyOf,Typeable,module Data.IsTy) where


import Data.Typeable (Typeable,TypeRep,typeOf)
import Unsafe.Coerce (unsafeCoerce)

import Data.Proof.EQ ((:=:)(..))

import Data.IsTy

-- | Phantom type wrapper around a 'TypeRep'
data Ty a = Ty { tyRep :: TypeRep }

instance Show (Ty a) where show = show . tyRep

instance IsTy Ty where
  Ty a `tyEq` Ty b | a == b    = unsafeCoerce (Just Refl)
                   | otherwise = Nothing

ty :: Typeable a => Ty a
ty = tyOf (undefined :: a)

-- | The 'Ty' of a value
tyOf :: Typeable a => a -> Ty a
tyOf a = Ty (typeOf a)
