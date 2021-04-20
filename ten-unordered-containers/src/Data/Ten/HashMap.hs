-- Copyright 2020-2021 Google LLC
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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A hash map linking keys' and values' type parameters existentially.

module Data.Ten.HashMap
         ( HashMap10
         , empty, insert, lookup, findWithDefault
         ) where

import Prelude hiding (lookup)

import Data.Maybe (fromMaybe)
import Data.Type.Equality ((:~:)(..), TestEquality(..))
import GHC.Generics (Generic1)

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Wrapped (Wrapped1(..))

import Data.Ten.Exists (Exists(..))
import Data.Ten.Foldable (Foldable10(..))
import Data.Ten.Functor (Functor10(..))
import Data.Ten.Traversable (Traversable10(..))

data Entry k m = forall a. Entry !(k a) (m a)

instance Functor10 (Entry k) where fmap10 f (Entry k m) = Entry k (f m)
instance Foldable10 (Entry k) where foldMap10 f (Entry _ m) = f m
instance Traversable10 (Entry k) where
  mapTraverse10 r f (Entry k m) = r . Entry k <$> f m

-- | A "dependent" hash map, where elements' type parameters match their keys'.
newtype HashMap10 k m = HashMap10 (HashMap (Exists k) (Entry k m))
  deriving Generic1
  deriving
    ( Functor10, Foldable10, Traversable10
    ) via Wrapped1 Generic1 (HashMap10 k)

-- | An empty 'HashMap10'.
empty :: HashMap10 k m
empty = HashMap10 HM.empty

-- | Insert a new pair into a 'HashMap10'
insert
  :: forall a k m
   . (TestEquality k, forall x. Hashable (k x), forall x. Eq (k x))
  => k a -> m a -> HashMap10 k m -> HashMap10 k m
insert k m (HashMap10 h) = HashMap10 $ HM.insert (Exists k) (Entry k m) h

fromEntry
  :: forall a k m
   . TestEquality k
  => k a -> Entry k m -> m a
fromEntry k (Entry k' m) = case testEquality k k' of
  Just Refl -> m
  Nothing -> error "Internal error: key mapped to entry of the wrong type."

-- | Find an entry based on its key, if present.
lookup
  :: forall a k m
   . (TestEquality k, forall x. Hashable (k x), forall x. Eq (k x))
  => k a -> HashMap10 k m -> Maybe (m a)
lookup k (HashMap10 h) = fromEntry k <$> HM.lookup (Exists k) h

-- | Find an entry based on its key, or return the given fallback value.
findWithDefault
  :: forall a k m
   . (TestEquality k, forall x. Hashable (k x), forall x. Eq (k x))
  => m a -> k a -> HashMap10 k m -> m a
findWithDefault m k = fromMaybe m . lookup k
