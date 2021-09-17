-- Copyright 2018-2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | A 'Functor10' made by applying the argument to an existential type.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Ten.Exists (Exists(..)) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..), TestEquality(..))

import Data.GADT.Compare (GEq(..), GCompare(..), GOrdering(..))
import Data.Hashable (Hashable(..))
import Data.Portray (Portray(..), Portrayal(..))
import Data.Portray.Diff (Diff(..), diffVs)

import Data.Ten.Functor (Functor10(..))
import Data.Ten.Foldable (Foldable10(..))
import Data.Ten.Traversable (Traversable10(..))

-- | A 'Functor10' made by applying the argument to an existential type.
data Exists (m :: k -> Type) where
  Exists :: forall a m. m a -> Exists m

deriving stock instance (forall a. Show (m a)) => Show (Exists m)

instance GEq m => Eq (Exists m) where
  Exists x == Exists y = case geq x y of
    Nothing -> False
    Just _ -> True

instance GCompare m => Ord (Exists m) where
  compare (Exists x) (Exists y) = case gcompare x y of
    GLT -> LT
    GEQ -> EQ
    GGT -> GT

instance (forall a. Hashable (m a)) => Hashable (Exists m) where
  hashWithSalt s (Exists ka) = hashWithSalt s ka

instance (forall a. Portray (m a)) => Portray (Exists m) where
  portray (Exists x) = Apply (Name "Exists") [portray x]

-- N.B. we do actually want TestEquality rather than GEq here, because we want
-- to diff same-typed-but-not-equal values according to their Diff instances.
instance (TestEquality m, forall a. Portray (m a), forall a. Diff (m a))
      => Diff (Exists m) where
  diff (Exists x) (Exists y) = case testEquality x y of
    Just Refl -> diff x y
    Nothing   -> Just $ portray x `diffVs` portray y

instance Functor10 Exists where
  fmap10 f (Exists x) = Exists (f x)

instance Foldable10 Exists where
  foldMap10 f (Exists x) = f x

instance Traversable10 Exists where
  mapTraverse10 r f (Exists x) = r . Exists <$> f x
