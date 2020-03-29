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
    Name()
  , AnyName()
  , strToName
) where

import           NoName.Nat

import           Data.Text          (Text)
import           Data.Type.Equality
import           Numeric.Natural
import           Type.Reflection


----------------------------------------------------------------------
---                          The Name Type                         ---
----------------------------------------------------------------------


data Name a (n :: Nat) where
  BoundName :: !Natural -- ^ Index within binder (relevant for telescopes)
            -> !(Fin n) -- ^ de Brujin index
            -> Name a ('S n)
  FreeName  :: !Natural -- ^ Globally unique identifier.
            -> !Text -> Name a n
  deriving (Typeable)

deriving instance Eq (Name a n)
deriving instance Ord (Name a n)


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

strToName :: Text -> Name a n
strToName s = FreeName 0 s
