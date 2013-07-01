{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds, KindSignatures
           , FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.IsTy
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Type class for typed type representations
----------------------------------------------------------------------

module Data.IsTy (IsTy(..)) where

import GHC.Prim (Constraint)

import Data.Proof.EQ ((:=:))

class Yes (f :: * -> *) a
instance Yes f a

-- | Type class for typed type representations
class IsTy ty where
  type IsTyConstraint ty z :: Constraint
  type IsTyConstraint ty z = Yes ty z
  tyEq :: (IsTyConstraint ty a, IsTyConstraint ty b) =>
          ty a -> ty b -> Maybe (a :=: b)
