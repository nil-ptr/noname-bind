{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  -- * Exported Items

) where

import           NoName.Nat

import           Data.Type.Equality
import           Numeric.Natural
import           Type.Reflection

data Name s a (n :: Nat) where
  BoundName :: !Natural -- ^ Index within binder (relevant for telescopes)
            -> !(Fin n) -- ^ de Brujin index
            -> Name s a ('S n)
  FreeName  :: !Natural -- ^ Globally unique identifier.
            -> !s -> Name s a n
  deriving (Typeable)

deriving instance Eq s => Eq (Name s a n)
deriving instance Ord s => Ord (Name s a n)


data AnyName s where
  MkAnyName :: (Typeable a, Typeable n) => Name s a n -> AnyName s
  deriving (Typeable)

instance (Eq s, Typeable s) => Eq (AnyName s) where
  (MkAnyName an1) == (MkAnyName an2) =
    case testEquality (typeOf an1) (typeOf an2) of
      Just Refl -> True
      _         -> False

instance (Ord s, Typeable s) => Ord (AnyName s) where
  compare (MkAnyName an1) (MkAnyName an2) =
    let t1 = typeOf an1
        t2 = typeOf an2
    in
    case testEquality t1 t2 of
      -- Same type => compare them at that type
      Just Refl -> compare an1 an2
      -- Different types => compare as SomeTypeRep
      _         -> compare (SomeTypeRep t1) (SomeTypeRep t2)
