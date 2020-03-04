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
  -- * Exported Items
  Internal.Nat(..),
  Internal.Fin(),
  pattern Internal.FS,
  pattern Internal.FZ,
  SNat(..),
  Internal.finToNatural
) where

import           NamelessB.Nat.Internal as Internal

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

class KnownSNat (n :: Nat) where
  snat :: SNat n

instance KnownSNat 'Z where
  snat = SZ
  {-# INLINE snat #-}

instance KnownSNat n => KnownSNat ('S n) where
  snat = SS snat
  {-# INLINE snat #-}

instance Show (SNat n) where
  showsPrec _ SZ      = showString "SZ"
  showsPrec i (SS sn) = showParen (i > app_prec) $
    showString "SS " . showsPrec (app_prec + 1) sn
    where app_prec = 10
  {-# INLINABLE showsPrec #-}
