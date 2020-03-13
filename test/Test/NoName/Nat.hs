{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Test.NoName.Nat
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for the NoName.Nat module.
module Test.NoName.Nat
  (
    natModuleSpec
  ) where

import           Numeric.Natural
import           Test.Hspec
import           Test.QuickCheck

import           NoName.Nat
import qualified NoName.Nat.Internal     as NI

import           Test.NoName.Nat.Orphans

----------------------------------------------------------------------
---                             Helpers                            ---
----------------------------------------------------------------------

--- types ------------------------------------------------------------

type AddOne n = 'S n
type AddTwo n = 'S ('S n)
type AddFive n = AddTwo (AddTwo (AddOne n))
type AddTen n = AddFive (AddFive n)

type Ten = AddTen 'Z

--- patterns ---------------------------------------------------------

pattern SomeSNat :: SNat n -> ForSomeNat SNat
pattern SomeSNat x <- MkForSomeNat x

pattern SomeTrueSNat :: NI.TrueSNat n -> ForSomeNat NI.TrueSNat
pattern SomeTrueSNat x <- MkForSomeNat x


----------------------------------------------------------------------
---                              Tests                             ---
----------------------------------------------------------------------

natModuleSpec :: Spec
natModuleSpec = describe "NoName.Nat" $ do
  snatSpec
  finSpec

snatSpec :: Spec
snatSpec = describe "NoName.Nat.SNat" $ do
  it "should be possible to roundtrip SNat -> TrueSNat -> SNat" $
    property $ \(SomeSNat x) ->
                 x === (NI.trueSNatToSNat . NI.snatToTrueSNat) x

  it "should be possible to roundtrip TrueSNat -> SNat -> TrueSNat" $
    property $ \(SomeTrueSNat x) ->
                 x === (NI.snatToTrueSNat . NI.trueSNatToSNat) x

finSpec :: Spec
finSpec = describe "NoName.Nat.Fin" $ do
  it "should never exceed its typelevel bound" $
    property $ \x ->
                 (finToNatural :: Fin Ten -> Natural) x <= 10
