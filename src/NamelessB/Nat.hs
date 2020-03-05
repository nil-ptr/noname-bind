{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PatternSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NamelessB.Nat
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Nat' and 'Fin' types.
module NamelessB.Nat
  (
  Internal.Nat(..)
  , Internal.Fin()
  , pattern Internal.FS
  , pattern Internal.FZ
  , Internal.finToNatural
  , Internal.SNat
  , pattern Internal.SZ
  , pattern Internal.SS
  , Internal.snatToNatural
  , Internal.snatToFin
) where

import           NamelessB.Nat.Internal as Internal
