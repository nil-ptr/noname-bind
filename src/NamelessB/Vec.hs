{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NamlessB.Vec
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Length indexed lists: 'Vec's.
module NamelessB.Vec
  (
  Vec(..),
  safeIndex,
  index
) where

import           Numeric.Natural

import           NamelessB.Nat



data Vec t (n :: Nat) where
  VNil :: Vec t 'Z
  VCons :: t -> Vec t n -> Vec t ('S n)

safeIndex :: Vec t ('S n) -> Fin n -> t
safeIndex (VCons x _) FZ       = x
safeIndex (VCons _ xs) (FS fn) = safeIndex xs fn

unsafeIndex :: Vec t ('S n) -> Natural -> t
unsafeIndex (VCons x _) 0              = x
unsafeIndex (VCons _ xs@(VCons _ _)) i = unsafeIndex xs (i - 1)
unsafeIndex (VCons _ VNil) i = error $
  "index '" ++ show i ++ "' out of bounds in 'unsafeIndex'"

index :: Vec t ('S n) -> Fin n -> t
index (VCons x _) FZ                   = x
index (VCons _ xs@(VCons _ _)) (FS fn) = unsafeIndex xs (finToNatural fn)
