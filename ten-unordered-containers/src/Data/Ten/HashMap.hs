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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | A hash map linking keys' and values' type parameters existentially.

module Data.Ten.HashMap
         ( -- * HashMap10
           HashMap10, (:**)(..)
         , empty, insert, lookup, findWithDefault
         , toList, fromList
           -- * Miscellaneous
         , Hashable1, Show1, Portray1, Diff1
         ) where

import Prelude hiding (lookup)

import qualified Data.Foldable as F
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup (Any(..), All(..))
import Data.Type.Equality ((:~:)(..), TestEquality(..))
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
import GHC.Generics (Generic1, (:.:)(..))

import Data.GADT.Compare (GEq(..))
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Portray (Portray(..), Portrayal(..))
import Data.Portray.Diff (Diff(..), diffVs)
import Data.Wrapped (Wrapped1(..))

import Data.Ten.Entails (Entails(..), (:!:))
import Data.Ten.Exists (Exists(..))
import Data.Ten.Foldable (Foldable10(..))
import Data.Ten.Foldable.WithIndex (Foldable10WithIndex(..))
import Data.Ten.Functor (Functor10(..))
import Data.Ten.Functor.WithIndex (Index10, Functor10WithIndex(..))
import Data.Ten.Traversable (Traversable10(..))
import Data.Ten.Traversable.WithIndex (Traversable10WithIndex(..))
import Data.Ten.Sigma ((:**)(..))

type Hashable1 k = forall x. Hashable (k x)
type Show1 k = forall x. Show (k x)
type Portray1 k = forall x. Portray (k x)
type Diff1 k = forall x. Diff (k x)

type instance Index10 (HashMap10 k) = k

-- | A "dependent" hash map, where elements' type parameters match their keys'.
newtype HashMap10 k m = HashMap10 (HashMap (Exists k) (k :** m))
  deriving Generic1
  deriving
    ( Functor10, Foldable10, Traversable10
    ) via Wrapped1 Generic1 (HashMap10 k)
  deriving
    ( Functor10WithIndex, Foldable10WithIndex, Traversable10WithIndex
    ) via HashMap (Exists k) :.: ((:**) k)

deriving stock
  instance (GEq k, Entails k (Eq :!: m)) => Eq (HashMap10 k m)

deriving stock
  instance (Show1 k, Entails k (Show :!: m)) => Show (HashMap10 k m)

instance (GEq k, Hashable1 k) => IsList (HashMap10 k m) where
  type Item (HashMap10 k m) = k :** m
  toList = toList
  fromList = fromList

instance (Portray1 k, Entails k (Portray :!: m))
      => Portray (HashMap10 k m) where
  portray = Apply (Name "fromList") . pure . portray . toList

data EntryDiff a = InLeft a | InBoth a a | InRight a

diffToM :: Maybe Portrayal -> ((Any, All), Maybe Portrayal)
diffToM = \case
  Nothing -> ((Any False, All False), Nothing)
  Just d  -> ((Any True, All True), Just d)

diffM :: (Diff a, Portray a) => EntryDiff a -> ((Any, All), Maybe Portrayal)
diffM e = diffToM $ case e of
  InLeft x -> Just $ portray x `diffVs` Opaque "_"
  InBoth x y -> diff x y
  InRight y -> Just $ Opaque "_" `diffVs` portray y

instance ( TestEquality k, GEq k, Hashable1 k, Portray1 k, Diff1 k
         , Entails k (Portray :!: m), Entails k (Diff :!: m)
         )
      => Diff (HashMap10 k m) where
  diff (HashMap10 l) (HashMap10 r) =
    if anyDiff
      then
        Just $ Apply (Name "fromList") $ pure $ List $
        (if allDiff then id else (++ [Opaque "..."])) $
        catMaybes $ F.toList diffs
      else Nothing
   where
    ((Any anyDiff, All allDiff), diffs) =
      traverse diffM $
      HM.unionWith combine (InLeft <$> l) (InRight <$> r)

    combine (InLeft x) (InRight y) = InBoth x y
    combine _ _ = error "Impossible"

-- | An empty 'HashMap10'.
empty :: HashMap10 k m
empty = HashMap10 HM.empty

-- | Insert a new pair into a 'HashMap10'
insert
  :: forall a k m
   . (GEq k, Hashable1 k)
  => k a -> m a -> HashMap10 k m -> HashMap10 k m
insert k m (HashMap10 h) = HashMap10 $ HM.insert (Exists k) (k :** m) h

fromEntry
  :: forall a k m
   . GEq k
  => k a -> k :** m -> m a
fromEntry k (k' :** m) = case geq k k' of
  Just Refl -> m
  Nothing -> error "Internal error: key mapped to entry of the wrong type."

-- | Find an entry based on its key, if present.
lookup
  :: forall a k m
   . (GEq k, Hashable1 k)
  => k a -> HashMap10 k m -> Maybe (m a)
lookup k (HashMap10 h) = fromEntry k <$> HM.lookup (Exists k) h

-- | Find an entry based on its key, or return the given fallback value.
findWithDefault
  :: forall a k m
   . (GEq k, Hashable1 k)
  => m a -> k a -> HashMap10 k m -> m a
findWithDefault m k = fromMaybe m . lookup k

-- | Convert to a list of (':**') in unspecified order.
toList :: HashMap10 k m -> [k :** m]
toList (HashMap10 m) = F.toList m

-- | Build a map from a list of (k ':**' m) entries.
fromList :: (GEq k, Hashable1 k) => [k :** m] -> HashMap10 k m
fromList = HashMap10 . HM.fromList . map (\ e@(k :** _) -> (Exists k, e))
