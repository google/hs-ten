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

import Data.Hashable (Hashable(..))
import Data.Portray (Portray(..), Portrayal(..))
import Data.Portray.Pretty (WrappedPortray(..))
import Text.PrettyPrint.HughesPJClass (Pretty)

import Data.Ten.Functor (Functor10(..))
import Data.Ten.Foldable (Foldable10(..))
import Data.Ten.Traversable (Traversable10(..))

-- | A 'Functor10' made by applying the argument to an existential type.
data Exists (m :: k -> Type) where
  Exists :: forall a m. m a -> Exists m

deriving stock instance (forall a. Show (m a)) => Show (Exists m)

-- | Test type-equality, then test equality according to 'Eq'.
--
-- This is because 'testEquality' returning 'Just' might not imply ('=='): you
-- could have e.g. @data Thing a where Thing :: Int -> Thing Int@, with
-- @testEquality (Thing _) (Thing _) = Just Refl@, but @Thing x == Thing y = x
-- == y@.  This might be considered bad form, but the documentation of
-- 'TestEquality' doesn't currently rule it out.
instance (TestEquality m, forall a. Eq (m a)) => Eq (Exists m) where
  Exists x == Exists y = case testEquality x y of
    Nothing -> False
    Just Refl -> x == y

instance (forall a. Hashable (m a)) => Hashable (Exists m) where
  hashWithSalt s (Exists ka) = hashWithSalt s ka

instance (forall a. Portray (m a)) => Portray (Exists m) where
  portray (Exists x) = Apply (Atom "Exists") [portray x]

deriving via WrappedPortray (Exists m)
  instance (forall a. Portray (m a)) => Pretty (Exists m)

instance Functor10 Exists where
  fmap10 f (Exists x) = Exists (f x)

instance Foldable10 Exists where
  foldMap10 f (Exists x) = f x

instance Traversable10 Exists where
  mapTraverse10 r f (Exists x) = r . Exists <$> f x
