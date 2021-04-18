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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides an analog of @Representable@ over arity-1 type constructors.

module Data.Ten.Representable
         ( Representable10(..), ix10
         , imap10, ifoldMap10, ifoldl10, ifoldr10, itraverse10
         , rep10, rep10'
         , distributeRep10, collectRep10
         ) where

import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.Monoid (Dual(..), Endo(..))
import GHC.Generics ((:.:)(..))

import Control.Lens (Lens', Getting, lens, view)

import Data.Ten.Applicative (Applicative10)
import Data.Ten.Foldable (Foldable10, fold10)
import Data.Ten.Traversable (Traversable10, sequenceA10)

(.:) :: (q -> r) -> (a -> b -> q) -> a -> b -> r
(.:) = (.) . (.)

-- | Analog of 'Data.Functor.Rep.Representable' over arity-1 type constructors.
--
-- If @f@ is @Representable10@, then a value of type @f m@ is isomorphic to a
-- function @forall a. Rep10 f a -> m a@.  This essentially means it can be
-- thought of as a fixed-shape record with a wrapper type applied to all of its
-- fields.
--
-- This is also equivalent to a total dependent map from @Rep10 f@ to @m@
-- ("total" meaning that every "key" has a "value").
class Applicative10 f => Representable10 (f :: (k -> Type) -> Type) where
  -- | The "index" type of an @f@ "container".
  --
  -- This is a type that behaves like a GADT, with a value for each possible
  -- "position" of an @m a@ in @f m@ and the parameter type(s) @a@ it can have.
  type Rep10 f :: k -> Type

  -- | Given an @f m@ and a @Rep10 f a@ "index" into it, extract the @m a@.
  index10 :: f m -> Rep10 f a -> m a

  -- | Build an @f m@ by applying a parametric function to each "index".
  tabulate10 :: (forall a. Rep10 f a -> m a) -> f m

  -- | Update an @f m@ at a given index.
  update10 :: Rep10 f a -> m a -> f m -> f m

-- | Turn a record field selector into a 'Rep10'.
--
-- See also 'rep10'.
rep10' :: Representable10 f => (f (Rep10 f) -> Rep10 f a) -> Rep10 f a
rep10' = ($ tabulate10 id)

-- | Turn a record field lens into a 'Rep10'.
--
-- Since 'tabulate10' can give us a record of 'Rep10's, all we have to do to
-- convert a lens into a 'Rep10' is use 'view' to extract the desired 'Rep10'.
rep10
  :: Representable10 f
  => Getting (Rep10 f a) (f (Rep10 f)) (Rep10 f a) -> Rep10 f a
rep10 l = rep10' (view l)

-- | A 'Control.Lens.Lens' to the field identified by a given 'Rep10'.
ix10 :: Representable10 f => Rep10 f a -> Lens' (f m) (m a)
ix10 i = lens (`index10` i) (flip (update10 i))

-- | 'Data.Ten.Functor10.fmap10' with an index parameter.
--
-- The additional 'Rep10' parameter is a GADT-like type that identifies the
-- current field within @f@ and its type.  Frequently this will be @Field10 f@,
-- but it may also be an actual hand-coded GADT.
imap10
  :: Representable10 f
  => (forall a. Rep10 f a -> m a -> n a) -> f m -> f n
imap10 f fm = tabulate10 (\i -> f i (fm `index10` i))

-- | 'Data.Ten.Foldable.foldMap10' with an index parameter.
ifoldMap10
  :: (Monoid w, Foldable10 f, Representable10 f)
  => (forall a. Rep10 f a -> m a -> w) -> f m -> w
ifoldMap10 f fm = fold10 $ imap10 (Const .: f) fm

-- | 'Data.Ten.Foldable.foldl10' with an index parameter.
ifoldl10
  :: (Foldable10 f, Representable10 f)
  => (forall a. Rep10 f a -> b -> m a -> b) -> b -> f m -> b
ifoldl10 f z fm = appEndo (ifoldMap10 (\i x -> Endo (\b -> f i b x)) fm) z

-- | 'Data.Ten.Foldable.foldr10' with an index parameter.
ifoldr10
  :: (Foldable10 f, Representable10 f)
  => (forall a. Rep10 f a -> m a -> b -> b) -> b -> f m -> b
ifoldr10 f z fm = flip appEndo z $ getDual $
  ifoldMap10 (\i x -> Dual $ Endo (f i x)) fm

-- | 'Data.Ten.Traversable.traverse10' with an index parameter.
itraverse10
  :: (Applicative f, Traversable10 t, Representable10 t)
  => (forall a. Rep10 t a -> m a -> f (n a))
  -> t m -> f (t n)
itraverse10 f fm = sequenceA10 $ imap10 (Comp1 .: f) fm

-- | Analog of 'Data.Functor.Rep.distributeRep' for 'Representable10'.
--
-- Pulls a fixed record shape to the outside of any functor.
distributeRep10
  :: (Representable10 f, Functor w)
  => w (f m) -> f (w :.: m)
distributeRep10 wfm = tabulate10 (\r -> Comp1 $ (`index10` r) <$> wfm)

-- | Analog of 'Data.Functor.Rep.collectRep' for 'Representable10'.
--
-- Gathers a fixed record shape mapped over the elements of any functor.
collectRep10
  :: (Representable10 f, Functor w)
  => (a -> f m) -> w a -> f (w :.: m)
collectRep10 f wa = distributeRep10 (f <$> wa)
