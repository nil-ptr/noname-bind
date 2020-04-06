{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RoleAnnotations #-}
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

data Bind p t k where
  Bind :: Pattern p m -> t (NatPlus m n) -> Bind p t n

mkPattern :: (t n -> Vec (Name (t n) (NatPlus m n)) m)
          -> t n
          -> (Pattern (t n) m, Vec (Name (t n) (NatPlus m n)) m)
mkPattern findNames patToBe = (MkPattern patToBe, findNames patToBe)
