{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE ViewPatterns      #-}

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
  -- * Nat
  Nat(..)
  -- * Fin
  -- ** Based on Numeric.Natural
  , Fin(..)
  , pattern FS
  , pattern FZ
  , finToNatural
  -- ** True Inductive Fin
  -- Exported for testing
  , TrueFin(..)
  , trueFinToNatural
  , trueFinToFin

  -- * SNat
  -- ** Based on Numeric.Natural
  , SNat(..)
  , pattern SZ
  , pattern SS
  , snatToNatural
  , snatToFin
  -- ** True Inductive SNat
  -- Exported for testing
  , TrueSNat(..)
  , trueSNatToNatural
  , trueSNatToSNat
  , trueSNatToTrueFin

) where

import           Numeric.Natural as N
import qualified Unsafe.Coerce   as Coerce (unsafeCoerce)

-- | Inductively defined naturals. Mainly intended for use at the type level.
data Nat = Z | S Nat


----------------------------------------------------------------------
---                          The Fin Type                          ---
----------------------------------------------------------------------

-- | A 'Fin' is a natural number guaranteed to be less than or equal
-- to its type level bound.
newtype Fin (n :: Nat) = MkFin Natural
type role Fin nominal

-- | Implementation detail. Used in the definition of 'matchFS', which
-- is in turn used in the definition of 'FS'.
data FinView (m :: Nat) (n :: Nat) where
  MkFinView :: (m ~ 'S n) => !(Fin n) -> FinView m n

-- | Only inteded for internal use in 'evilMkFinView'. Use at own risk.
evilSpoofFinView :: FinView ('S n) n -> FinView m n
evilSpoofFinView = Coerce.unsafeCoerce
{-# INLINE evilSpoofFinView #-}

-- | Only intended for internal use in 'matchFS'. Use at own risk.
evilMkFinView :: Fin n -> FinView m n
evilMkFinView x = evilSpoofFinView (MkFinView x)
{-# INLINE evilMkFinView #-}

-- | This function makes benign, but nevertheless EVIL use of
-- 'unsafeCoerce' to make up a bogus equality constraint, required in
-- the definition of the 'FS' pattern synonym. This constraint is
-- perfectly reasonable so long as one does not have access to the
-- constructor of 'Fin', but it /is/ technically bogus.
matchFS :: Fin m -> Maybe (FinView m n)
matchFS (MkFin 0) = Nothing
matchFS (MkFin x) = Just (evilMkFinView $ MkFin (x - 1))
{-# INLINABLE matchFS #-}

-- | This is @ O(1) @.
finToNatural :: Fin n -> Natural
finToNatural (MkFin x) = x
{-# INLINE finToNatural #-}

-- | Zero is less than or equal to any given bound.
pattern FZ :: Fin n
pattern FZ = MkFin 0

-- | The successor of a 'Fin' @ x @ has the successor of the bound of
-- @ x @ as its bound.
--
-- The somewhat funky looking contraints on this pattern gives it
-- semantics that mimic a GADT constructor. That is, matching on it
-- brings the @ (n ~ 'S m) @ constraint into scope.
pattern FS :: () => (n ~ 'S m) => Fin m -> Fin n
pattern FS x <- (matchFS -> Just (MkFinView x))
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

--- TrueFin ----------------------------------------------------------

data TrueFin (n :: Nat) where
  TrueFZ :: TrueFin n
  TrueFS :: !(TrueFin n) -> TrueFin ('S n)

trueFinToNatural :: TrueFin n -> Natural
trueFinToNatural TrueFZ      = 0
trueFinToNatural (TrueFS fn) = 1 + trueFinToNatural fn
{-# INLINABLE trueFinToNatural #-}

instance Show (TrueFin n) where
  showsPrec _ TrueFZ = showString "TrueFZ"
  showsPrec i (TrueFS fn) = showParen (i > app_prec) $
    showString "TrueFS " . showsPrec (app_prec + 1) fn
    where app_prec = 10
  {-# INLINABLE showsPrec #-}

trueFinToFin :: TrueFin n -> Fin n
trueFinToFin TrueFZ       = FZ
trueFinToFin (TrueFS tfn) = FS (trueFinToFin tfn)

----------------------------------------------------------------------
---                          The SNat Type                         ---
----------------------------------------------------------------------

newtype SNat (n :: Nat) = MkSNat Natural
type role SNat nominal

data SNatSSView (m :: Nat) (n :: Nat) where
   MkSNatSSView :: (m ~ 'S n) => !(SNat n) -> SNatSSView m n

evilSpoofSNatSSView :: SNatSSView ('S n) n -> SNatSSView m n
evilSpoofSNatSSView = Coerce.unsafeCoerce
{-# INLINE evilSpoofSNatSSView #-}

evilMkSNatSSView :: SNat n -> SNatSSView m n
evilMkSNatSSView x = evilSpoofSNatSSView (MkSNatSSView x)
{-# INLINE evilMkSNatSSView #-}

matchSS :: SNat m -> Maybe (SNatSSView m n)
matchSS (MkSNat 0) = Nothing
matchSS (MkSNat x) = Just (evilMkSNatSSView $ MkSNat (x - 1))
{-# INLINABLE matchSS #-}


data SNatSZView (n :: Nat) where
  MkSNatSZView :: (n ~ 'Z) => SNatSZView n

evilSpoofSNatSZView :: SNatSZView 'Z -> SNatSZView n
evilSpoofSNatSZView = Coerce.unsafeCoerce
{-# INLINE evilSpoofSNatSZView #-}

evilMkSNatSZView :: SNatSZView n
evilMkSNatSZView = evilSpoofSNatSZView MkSNatSZView
{-# INLINE evilMkSNatSZView #-}

matchSZ :: SNat n -> Maybe (SNatSZView n)
matchSZ (MkSNat 0) = Just evilMkSNatSZView
matchSZ _          = Nothing

snatToNatural :: SNat n -> Natural
snatToNatural (MkSNat x) = x

pattern SZ :: () => (n ~ 'Z) =>  SNat n
pattern SZ <- (matchSZ -> Just (MkSNatSZView))
  where SZ  = MkSNat 0

pattern SS :: () => (n ~ 'S m) => SNat m -> SNat n
pattern SS x <- (matchSS -> Just (MkSNatSSView x))
  where SS x = MkSNat (1 + snatToNatural x)

{-# COMPLETE SZ, SS #-}

snatToFin :: SNat n -> Fin n
snatToFin (MkSNat x) = MkFin x
{-# INLINE snatToFin #-}


instance Show (SNat n) where
  showsPrec _ SZ = showString "SZ"
  showsPrec i (SS fn) = showParen (i > app_prec) $
    showString "SS " . showsPrec (app_prec + 1) fn
    where app_prec = 10
  {-# INLINABLE showsPrec #-}

--- True SNat --------------------------------------------------------

data TrueSNat (n :: Nat) where
  TrueSZ :: TrueSNat 'Z
  TrueSS :: !(TrueSNat n) -> TrueSNat ('S n)

trueSNatToNatural :: TrueSNat n -> Natural
trueSNatToNatural TrueSZ     = 0
trueSNatToNatural (TrueSS n) = 1 + trueSNatToNatural n

instance Show (TrueSNat 'Z) where
  showsPrec _ TrueSZ = showString "TrueSZ"

instance (Show (TrueSNat n)) => Show (TrueSNat ('S n)) where
  showsPrec i (TrueSS sn) = showParen (i > app_prec) $
    showString "TrueSS " . showsPrec (app_prec + 1) sn
    where app_prec = 10

trueSNatToSNat :: TrueSNat n -> SNat n
trueSNatToSNat TrueSZ       = SZ
trueSNatToSNat (TrueSS tsn) = SS (trueSNatToSNat tsn)

trueSNatToTrueFin :: TrueSNat n -> TrueFin n
trueSNatToTrueFin TrueSZ       = TrueFZ
trueSNatToTrueFin (TrueSS tsn) = TrueFS (trueSNatToTrueFin tsn)
