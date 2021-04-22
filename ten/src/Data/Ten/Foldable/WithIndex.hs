-- Copyright 2021 Google LLC
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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Ten.Foldable.WithIndex
         ( Index10, Foldable10WithIndex(..)
         , ifoldl10, ifoldr10, itraverse10_
         , foldMap10C, foldr10C, foldl10C, traverse10C_
         ) where

import Data.Monoid (Dual(..), Endo(..))

import GHC.Generics ((:.:)(..))

import Data.Ten.Entails (Entails(..), byEntailment)
import Data.Ten.Foldable (Foldable10(..))
import Data.Ten.Functor.WithIndex (Index10)

class Foldable10 f => Foldable10WithIndex f where
  ifoldMap10 :: Monoid w => (forall a. Index10 f a -> m a -> w) -> f m -> w

instance (Foldable g, Foldable10WithIndex f)
      => Foldable10WithIndex (g :.: f) where
  ifoldMap10 f (Comp1 gfm) = foldMap (ifoldMap10 f) gfm

-- | 'Data.Ten.Foldable.foldl10' with an index parameter.
ifoldl10
  :: Foldable10WithIndex f
  => (forall a. Index10 f a -> b -> m a -> b)
  -> b -> f m -> b
ifoldl10 f z fm = appEndo (ifoldMap10 (\i x -> Endo (\b -> f i b x)) fm) z

-- | 'Data.Ten.Foldable.foldr10' with an index parameter.
ifoldr10
  :: Foldable10WithIndex f
  => (forall a. Index10 f a -> m a -> b -> b) -> b -> f m -> b
ifoldr10 f z fm = flip appEndo z $ getDual $
  ifoldMap10 (\i x -> Dual $ Endo (f i x)) fm

-- | 'Data.Ten.Foldable.traverse10_' with an index parameter.
itraverse10_
  :: (Foldable10WithIndex f, Applicative g)
  => (forall a. Index10 f a -> m a -> g ())
  -> f m -> g ()
itraverse10_ f = ifoldl10 (\i a x -> a <* f i x) (pure ())

-- | 'foldMap10' with access to an instance for every element.
foldMap10C
  :: forall c f m w
   . (Entails (Index10 f) c, Foldable10WithIndex f, Monoid w)
  => (forall a. c a => m a -> w) -> f m -> w
foldMap10C f = ifoldMap10 (byEntailment @c f)

-- | 'foldr10' with access to an instance for every element.
foldr10C
  :: forall c f m b
   . (Entails (Index10 f) c, Foldable10WithIndex f)
  => (forall a. c a => m a -> b -> b) -> b -> f m -> b
foldr10C f = ifoldr10 (byEntailment @c f)

-- | 'foldl10' with access to an instance for every element.
foldl10C
  :: forall c f m b
   . (Entails (Index10 f) c, Foldable10WithIndex f)
  => (forall a. c a => b -> m a -> b) -> b -> f m -> b
foldl10C f = ifoldl10 (byEntailment @c f)

-- | 'traverse10_' with access to an instance for every element.
traverse10C_
  :: forall c f g m
   . (Entails (Index10 f) c, Applicative g, Foldable10WithIndex f)
  => (forall a. c a => m a -> g ()) -> f m -> g ()
traverse10C_ f = itraverse10_ (byEntailment @c f)
