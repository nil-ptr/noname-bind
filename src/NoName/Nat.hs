{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  NoName.Nat
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Nat' and 'Fin' types.
module NoName.Nat
  (
  Internal.Nat(..)
  , absurdZeroNotSucc
  , NatPlus
  , Internal.Fin()
  , pattern Internal.FS
  , pattern Internal.FZ
  , Internal.finToNatural
  , Internal.incrBound
  , Internal.SNat
  , pattern Internal.SZ
  , pattern Internal.SS
  , Internal.snatToNatural
  , Internal.snatToFin
  , ForSomeNat(..)
  , buildForSomeNat
) where

import           Data.Type.Equality
import           Data.Void
import           NoName.Nat.Internal as Internal
import           Numeric.Natural

----------------------------------------------------------------------
---                          Nat Equality                          ---
----------------------------------------------------------------------

absurdZeroNotSucc :: ('Z :~: 'S n) -> Void
absurdZeroNotSucc x = case x of {}

-- | A polykinded existential wrapper for 'Internal.Nat' indexed types
-- (kinds).
data ForSomeNat (t :: Nat -> k) where
  MkForSomeNat :: t n -> ForSomeNat t

instance (forall n. Show (t n)) => Show (ForSomeNat t) where
  showsPrec i (MkForSomeNat x) = showParen (i > app_prec) $
    showString "MkForSomeNat " . showsPrec (app_prec + 1) x
    where app_prec = 10

-- | Iteratively build a 'Internal.Nat' indexed value using the
-- supplied \"zero\" and \"successor\" functions.
--
-- E.g:
--
-- @
-- buildForSomeNat 4 SZ SS ≡ MkForSomeNat (SS (SS (SS (SS SZ))))
-- @
buildForSomeNat :: Natural -- ^ Size
                -> (t z) -- ^ "Zero"
                -> (forall k. t k -> t ('S k)) -- ^ "Successor"
                -> ForSomeNat t
buildForSomeNat 0 zero _ = MkForSomeNat zero
buildForSomeNat i zero successor =
  case buildForSomeNat (i - 1) zero successor of
    MkForSomeNat x -> MkForSomeNat (successor x)
{-# INLINABLE buildForSomeNat #-}


type family NatPlus (m :: Nat) (n :: Nat) :: Nat where
  NatPlus ('S m) x = NatPlus m ('S x)
  NatPlus 'Z x = x
