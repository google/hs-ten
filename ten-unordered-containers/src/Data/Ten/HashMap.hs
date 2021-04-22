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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | A hash map linking keys' and values' type parameters existentially.

module Data.Ten.HashMap
         ( HashMap10, Entry(..)
         , empty, insert, lookup, findWithDefault
         , toList, fromList
         ) where

import Prelude hiding (lookup)

import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import Data.Type.Equality ((:~:)(..), TestEquality(..))
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
import GHC.Generics (Generic1, (:.:)(..))

import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Portray (Portray(..), Portrayal(..), infixr_)
import Data.Wrapped (Wrapped1(..))

import Data.Ten.Entails (Entails(..), Dict1(..), (:!:))
import Data.Ten.Exists (Exists(..))
import Data.Ten.Foldable (Foldable10(..))
import Data.Ten.Foldable.WithIndex (Foldable10WithIndex(..))
import Data.Ten.Functor (Functor10(..))
import Data.Ten.Functor.WithIndex (Index10, Functor10WithIndex(..))
import Data.Ten.Traversable (Traversable10(..))
import Data.Ten.Traversable.WithIndex (Traversable10WithIndex(..))

infixr 5 :=>
data Entry k m = forall a. !(k a) :=> m a

instance (forall a. Portray (k a), Entails k (Portray :!: m))
      => Portray (Entry k m) where
  portray (k :=> m) = Binop ":=>" (infixr_ 5)
    (portray k)
    (case entailment @k @(Portray :!: m) k of Dict1 -> portray m)

instance Functor10 (Entry k) where fmap10 f (k :=> m) = k :=> f m
instance Foldable10 (Entry k) where foldMap10 f (_ :=> m) = f m
instance Traversable10 (Entry k) where
  mapTraverse10 r f (k :=> m) = r . (k :=>) <$> f m

type instance Index10 (Entry k) = k
instance Functor10WithIndex (Entry k) where imap10 f (k :=> m) = k :=> f k m
instance Foldable10WithIndex (Entry k) where ifoldMap10 f (k :=> m) = f k m
instance Traversable10WithIndex (Entry k) where
  imapTraverse10 r f (k :=> m) = r . (k :=>) <$> f k m

type Hashable1 k = forall x. Hashable (k x)
type Eq1 k = forall x. Eq (k x)

type instance Index10 (HashMap10 k) = k

-- | A "dependent" hash map, where elements' type parameters match their keys'.
newtype HashMap10 k m = HashMap10 (HashMap (Exists k) (Entry k m))
  deriving Generic1
  deriving
    ( Functor10, Foldable10, Traversable10
    ) via Wrapped1 Generic1 (HashMap10 k)
  deriving
    ( Functor10WithIndex, Foldable10WithIndex, Traversable10WithIndex
    ) via HashMap (Exists k) :.: Entry k

instance (TestEquality k, Eq1 k, Hashable1 k) => IsList (HashMap10 k m) where
  type Item (HashMap10 k m) = Entry k m
  toList = toList
  fromList = fromList

instance (forall a. Portray (k a), Entails k (Portray :!: m))
      => Portray (HashMap10 k m) where
  portray = Apply "fromList" . pure . portray . toList

-- | An empty 'HashMap10'.
empty :: HashMap10 k m
empty = HashMap10 HM.empty

-- | Insert a new pair into a 'HashMap10'
insert
  :: forall a k m
   . (TestEquality k, Hashable1 k, Eq1 k)
  => k a -> m a -> HashMap10 k m -> HashMap10 k m
insert k m (HashMap10 h) = HashMap10 $ HM.insert (Exists k) (k :=> m) h

fromEntry
  :: forall a k m
   . TestEquality k
  => k a -> Entry k m -> m a
fromEntry k (k' :=> m) = case testEquality k k' of
  Just Refl -> m
  Nothing -> error "Internal error: key mapped to entry of the wrong type."

-- | Find an entry based on its key, if present.
lookup
  :: forall a k m
   . (TestEquality k, Hashable1 k, Eq1 k)
  => k a -> HashMap10 k m -> Maybe (m a)
lookup k (HashMap10 h) = fromEntry k <$> HM.lookup (Exists k) h

-- | Find an entry based on its key, or return the given fallback value.
findWithDefault
  :: forall a k m
   . (TestEquality k, Hashable1 k, Eq1 k)
  => m a -> k a -> HashMap10 k m -> m a
findWithDefault m k = fromMaybe m . lookup k

-- | Convert to a list of 'Entry' in unspecified order.
toList :: HashMap10 k m -> [Entry k m]
toList (HashMap10 m) = F.toList m

-- | Build a map from a list of 'Entry'.
fromList :: (TestEquality k, Hashable1 k, Eq1 k) => [Entry k m] -> HashMap10 k m
fromList = HashMap10 . HM.fromList . map (\ e@(k :=> _) -> (Exists k, e))
