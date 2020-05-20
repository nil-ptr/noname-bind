{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeFamilies      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NoName.Bind
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Description forthcoming.
module NoName.Bind
  (
  -- * Exported Items

) where

import           NoName.Name
import           NoName.Nat
import           NoName.Vec

newtype Pattern a (n :: Nat) = MkPattern a
type role Pattern nominal nominal

data Bind m p t k where
  Bind :: Pattern p m -> t (NatPlus m n) -> Bind m p t n

mkPattern :: (t n -> Vec (Name (t n) (NatPlus m n)) m)
          -> t n
          -> (Pattern (t n) m, Vec (Name (t n) (NatPlus m n)) m)
mkPattern findNames patToBe = (MkPattern patToBe, findNames patToBe)



data VV m n = BV (Name (LExp m) n) | FV (FreeName (LExp m))

type family Obscure (m :: Nat) (n :: Nat) :: Nat where
  Obscure 'Z x = x
  Obscure x _ = x

mkVV :: Name (LExp m) n -> VV m (Obscure n m)
mkVV x = case boundedness x of
           Right IsFree     -> FV x
           Left IsSomeBound -> BV x

data LExp (m :: Nat) where
  Var :: VV n n -> LExp n
  App :: LExp n -> LExp n -> LExp n
  Lam :: Bind ('S 'Z) (VV n 'Z) LExp n -> LExp n



vx = mkVV (strToName "x")

foo :: LExp 'Z
foo = Lam (Bind (MkPattern vx) (App undefined undefined))

--geh = case foo of
--        Lam (Bind x y) -> _

---- In theory we should need / define these
-- nth :: Fin n -> Pattern t n -> Name t m
