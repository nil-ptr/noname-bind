{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NamelessB.Nat.Internal
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Internals of the 'Fin' type. The functions defined in this module
-- should be used with great care, as they may subvert invariants.
module NamelessB.Nat.Internal
  (
  -- * Exported Items
  Nat(..),
  Fin(..),
  pattern FS,
  pattern FZ,
  finToNatural,
  FView(..)
) where

import           Numeric.Natural as N
import qualified Unsafe.Coerce   as Coerce (unsafeCoerce)


data Nat = Z | S Nat

newtype Fin (n :: Nat) = MkFin Natural
type role Fin nominal

data FView (m :: Nat) (n :: Nat) where
  MkFView :: (m ~ 'S n) => (Fin n) -> FView m n

-- | This function makes benign, but nevertheless EVIL use of
-- 'unsafeCoerce' to make up a bogus equality constraint, required in
-- the definition of the 'FS' pattern synonym. This constraint is
-- perfectly reasonable so long as one does not have access to the
-- constructor of 'Fin', but it /is/ technically bogus.
matchS :: Fin m -> Maybe (FView m n)
matchS (MkFin 0) = Nothing
matchS (MkFin x) = Just (Coerce.unsafeCoerce $ MkFView $ MkFin (x - 1))
{-# INLINABLE matchS #-}

finToNatural :: Fin n -> Natural
finToNatural (MkFin x) = x
{-# INLINE finToNatural #-}

pattern FZ :: Fin n
pattern FZ = MkFin 0

pattern FS :: () => (n ~ 'S m) => Fin m -> Fin n
pattern FS x <- (matchS -> Just (MkFView x))
  where FS x = MkFin (1 + finToNatural x)

{-# COMPLETE FS, FZ #-}

-- | This instance is a lie. It will pretend that 'FZ' and 'FS' are
-- actual constructors rather than pattern synonyms.
instance Show (Fin n) where
  showsPrec _ FZ      = showString "FZ"
  showsPrec i (FS fn) = showParen (i > app_prec) $
    showString "FS " . showsPrec (app_prec + 1) fn
    where app_prec = 10
  {-# INLINABLE showsPrec #-}
