{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Test.NoName.Nat.Orphans
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Orphan instances required for QuickCheck testing.
module Test.NoName.Nat.Orphans where

import           NoName.Nat
import qualified NoName.Nat.Internal as NI

import           Numeric.Natural
import           Test.QuickCheck

----------------------------------------------------------------------
---                             Helpers                            ---
----------------------------------------------------------------------

toNatural :: Integer -> Natural
toNatural i = fromInteger @Natural $
  if i < 0
  then negate i
  else i


----------------------------------------------------------------------
---                               Fin                              ---
----------------------------------------------------------------------


instance NI.KnownSNat n => Arbitrary (Fin n) where
  arbitrary = let x = fromIntegral . snatToNatural $ NI.snat @ n
                  rng = choose @Integer (0,x)
              in (NI.MkFin . fromIntegral) <$> rng

instance Arbitrary (NI.TrueFin 'Z) where
  arbitrary = pure NI.TrueFZ

instance Arbitrary (NI.TrueFin n) => Arbitrary (NI.TrueFin ('S n)) where
  arbitrary = do
    fn <- arbitrary
    b <- arbitrary
    pure $
      if b
      then NI.TrueFS fn
      else NI.incrTrueFinBound fn

----------------------------------------------------------------------
---                              SNat                              ---
----------------------------------------------------------------------

instance Arbitrary (ForSomeNat SNat) where
  arbitrary = do
    x <- arbitrary
    pure (MkForSomeNat . NI.MkSNat . toNatural $ x)

instance Arbitrary (ForSomeNat NI.TrueSNat) where
  arbitrary = do
    x <- arbitrary
    pure $ buildForSomeNat (toNatural x) NI.TrueSZ NI.TrueSS
