{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NoName.Name
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Names.
module NoName.Name
  (
    -- * Names
    Name()
  , FreeName
  , strToName
  -- ** Boundedness of Names
  , IsSomeBound(..)
  , IsFree(..)
  , boundedness
  , absurdIsFreeIsSomeBound
  -- ** AnyName
  , AnyName()
  -- * Utilities
  , isFreeFromEq
  , isFreeToEq
  , isSomeBoundFromEq
  , useSomeBoundAsEq
) where

import           NoName.Nat

import           Data.Text          (Text)
import           Data.Type.Equality
import           Data.Void
import           Numeric.Natural
import           Type.Reflection

----------------------------------------------------------------------
---                          The Name Type                         ---
----------------------------------------------------------------------


-- | The 'Nat' index determines which binder this name belongs to. An
-- index of 'Z' indicates a free name.
data Name a (b :: Nat) where
  MkBoundName :: !Natural -- ^ Index within binder (relevant for telescopes)
            -> !(Fin n) -- ^ de Brujin index
            -> Name a ('S n)
  MkFreeName  :: !Natural -- ^ Globally unique identifier.
            -> !Text -> Name a 'Z
  deriving (Typeable)

data IsSomeBound (a :: Nat) where
  IsSomeBound :: (a ~ 'S n) => IsSomeBound a

data IsFree (a :: Nat) where
  IsFree :: (a ~ 'Z) => IsFree a

deriving instance Eq (Name a n)
deriving instance Ord (Name a n)

type FreeName a = Name a 'Z

--- Distinguishing Free and Bound Names ------------------------------

-- | Determine if a particular 'Name' is bound or not. Pattern
-- matching on the result of this function will refine the type of the
-- argument.
boundedness :: Name a b -> Either (IsSomeBound b) (IsFree b)
boundedness (MkFreeName _ _)  = Right IsFree
boundedness (MkBoundName _ _) = Left IsSomeBound

isFreeFromEq :: (x :~: 'Z) -> IsFree x
isFreeFromEq Refl = IsFree

isFreeToEq :: IsFree x -> x :~: 'Z
isFreeToEq IsFree = Refl

-- | Note that this tranformation is lossy: we forget the specific
-- bound and retain only the knowledge that it was /a/ bound.
isSomeBoundFromEq :: (x :~: 'S n) -> IsSomeBound x
isSomeBoundFromEq Refl = IsSomeBound

-- | I can't give you the equality directly because the @n@ is
-- existentially quantified inside the 'IsSomeBound' value.
useSomeBoundAsEq :: IsSomeBound x
                 -> (forall n. x :~: 'S n -> t)
                 -> t
useSomeBoundAsEq IsSomeBound f = f Refl

absurdIsFreeIsSomeBound :: IsFree x -> IsSomeBound x -> Void
absurdIsFreeIsSomeBound x y = useSomeBoundAsEq y $ \prf_y ->
  absurdZeroNotSucc (trans (sym (isFreeToEq x)) prf_y)


--- Existentials for Names -------------------------------------------

data AnyName where
  MkAnyName :: (Typeable a, Typeable n) => Name a n -> AnyName
  deriving (Typeable)

instance Eq AnyName where
  (MkAnyName an1) == (MkAnyName an2) =
    case testEquality (typeOf an1) (typeOf an2) of
      Just Refl -> True
      _         -> False

instance Ord AnyName where
  compare (MkAnyName an1) (MkAnyName an2) =
    let t1 = typeOf an1
        t2 = typeOf an2
    in
    case testEquality t1 t2 of
      -- Same type => compare them at that type
      Just Refl -> compare an1 an2
      -- Different types => compare as SomeTypeRep
      _         -> compare (SomeTypeRep t1) (SomeTypeRep t2)

----------------------------------------------------------------------
---                        Name Manipulation                       ---
----------------------------------------------------------------------

strToName :: Text -> FreeName a
strToName s = MkFreeName 0 s
