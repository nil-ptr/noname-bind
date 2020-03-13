{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
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

pattern SomeTrueFin :: NI.TrueFin n -> ForSomeNat NI.TrueFin
pattern SomeTrueFin x <- MkForSomeNat x

-- | Turn an 'Integer' into a 'Natural' by negating, if needed.
integerAsNatural :: Integer -> Natural
integerAsNatural i | i < 0 = fromInteger (negate i)
                   | otherwise = fromInteger i

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

  it "should be possible to convert an SNat to the corresponding Natural"  $
    property $ \(integerAsNatural -> x) ->
                case buildForSomeNat x SZ SS of
                  MkForSomeNat y -> x === NI.snatToNatural y

  describe "when converting to Natural" $ do
    it "should produce the same Natural if converted to TrueSNat in between" $
      property $ \(integerAsNatural -> x) ->
                   case buildForSomeNat x SZ SS of
                     MkForSomeNat y ->
                       NI.snatToNatural y
                       ===
                       (NI.trueSNatToNatural . NI.snatToTrueSNat) y
    it "should be possible to roundtrip via TrueSNat and get the same result" $
      property $ \(integerAsNatural -> x) ->
                   case buildForSomeNat x SZ SS of
                     MkForSomeNat y ->
                       NI.snatToNatural y
                       ===
                       (NI.snatToNatural
                        . NI.trueSNatToSNat
                        . NI.snatToTrueSNat) y

finSpec :: Spec
finSpec = describe "NoName.Nat.Fin" $ do
  it "should never exceed its typelevel bound" $
    property $ \x ->
                 (finToNatural :: Fin Ten -> Natural) x <= 10

  it "should never exceed the SNat with the same bound" $
    property $ \(SomeSNat x) ->
                 forAll (genFin x) $ \fn ->
                   finToNatural fn <= snatToNatural x

  let finToTrueFin :: Fin n -> NI.TrueFin n
      finToTrueFin FZ      = NI.TrueFZ
      finToTrueFin (FS fn) = NI.TrueFS (finToTrueFin fn)

  it "should be possible to roundtrip Fin -> TrueFin -> Fin" $
    property $ \(SomeSNat x) ->
                 forAll (genFin x) $ \fn ->
                   fn === (NI.trueFinToFin . finToTrueFin) fn

  it "should be possible to roundtrip TrueFin -> Fin -> TrueFin" $
    property $ \(integerAsNatural -> x) ->
                 forAll (genBuildable NI.TrueFZ NI.TrueFS x) $
                 \(SomeTrueFin fn) ->
                   fn === (finToTrueFin . NI.trueFinToFin) fn
