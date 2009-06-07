{-# LANGUAGE TypeOperators #-}
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


import Data.Typeable
import Data.Proof.EQ ((:=:))

-- | Type class for typed type representations
class IsTy ty where
  tyEq :: ty a -> ty b -> Maybe (a :=: b)
  ty   :: Typeable a => ty a
