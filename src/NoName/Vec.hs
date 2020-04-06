{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module NoName.Vec
  (
     Vec(..)
   , safeIndex
   , index
   , (!!)
) where

import           Numeric.Natural
import           Prelude             hiding ((!!))

import           NoName.Nat
import qualified NoName.Nat.Internal as UnsafeNat

----------------------------------------------------------------------
---                          The Vec Type                          ---
----------------------------------------------------------------------

-- | Length indexed lists.
data Vec t (n :: Nat) where
  VNil :: Vec t 'Z
  VCons :: t -> Vec t n -> Vec t ('S n)

-- | Treats the 'Fin' as inductively defined. Slower than 'index'.
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

-- | Infix version of 'index'.
(!!) :: Vec t ('S n) -> Fin n -> t
(!!) xs fn = index xs fn

infixl 9 !!

length :: Vec t m -> SNat m
length VNil         = SZ
length (VCons _ xs) = SS (NoName.Vec.length xs)

findFirstIndex :: forall t n.
                  (t -> Bool)
               -> Vec t ('S n)
               -> Maybe (t, Fin ('S n))
findFirstIndex p v = go 0 v
  where go :: forall m.  Natural -> Vec t ('S m) -> Maybe (t , Fin ('S n))
        go i (VCons x xs) = if p x
                              then Just (x, UnsafeNat.MkFin i)
                              else case xs of
                                     VNil        -> Nothing
                                     (VCons _ _) -> go (i+1) xs
