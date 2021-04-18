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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides an analog of @Representable@ over arity-1 type constructors.

module Data.Ten.Representable
         ( Representable10(..)
         , imap10, ifoldMap10, ifoldl10, ifoldr10, itraverse10
         , rep10'
         , distributeRep10, collectRep10
         , GTabulate10(..)
         ) where

import Data.Coerce (coerce)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.Monoid (Dual(..), Endo(..))
import Data.Type.Equality ((:~:)(..))
import GHC.Generics
         ( Generic1(..)
         , (:.:)(..), (:*:)(..)
         , M1(..), Rec1(..), U1(..)
         )

import Data.Wrapped (Wrapped1(..))

import Data.Functor.Rep (Representable(..))
import Data.Ten.Ap (Ap10(..))
import Data.Ten.Applicative (Applicative10(..))
import Data.Ten.Field (Field10(..))
import Data.Ten.Foldable (Foldable10(..), fold10)
import Data.Ten.Internal (starFst, starSnd)
import Data.Ten.Traversable (Traversable10(..), fsequenceA10)

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

-- | Turn a record field selector into a 'Rep10'.
--
-- See also 'Data.Ten.Lens.rep10'.
rep10' :: Representable10 f => (f (Rep10 f) -> Rep10 f a) -> Rep10 f a
rep10' = ($ tabulate10 id)

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
itraverse10 f fm = fsequenceA10 $ imap10 (Comp1 .: f) fm

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

class GTabulate10 (rec :: (k -> Type) -> Type) where
  gtabulate10 :: (forall a. Field10 rec a -> r a) -> rec r

instance Representable10 (Ap10 a) where
  type Rep10 (Ap10 a) = (:~:) a
  index10 (Ap10 x) Refl = x
  tabulate10 f = Ap10 (f Refl)

instance GTabulate10 U1 where
  gtabulate10 _ = U1
  {-# INLINE gtabulate10 #-}

instance Representable10 rec => GTabulate10 (Rec1 rec) where
  gtabulate10 r = Rec1 $
    tabulate10 (\i -> r (Field10 (\ (Rec1 f) -> index10 f i)))
  {-# INLINE gtabulate10 #-}

instance GTabulate10 rec => GTabulate10 (M1 k i rec) where
  gtabulate10 r = M1 $ gtabulate10 (r . coerce)

instance (GTabulate10 f, GTabulate10 g)
      => GTabulate10 (f :*: g) where
  gtabulate10 r = ftab :*: gtab
   where
    ftab = gtabulate10 $ \ (Field10 g) -> r $ Field10 $ g . starFst
    gtab = gtabulate10 $ \ (Field10 g) -> r $ Field10 $ g . starSnd
  {-# INLINE gtabulate10 #-}

instance (Representable f, GTabulate10 g) => GTabulate10 (f :.: g) where
  gtabulate10 r = Comp1 $
    tabulate $ \ i ->
    gtabulate10 $ \ (Field10 g) ->
    r $ Field10 (g . flip index i . unComp1)
  {-# INLINE gtabulate10 #-}

instance (Generic1 rec, Applicative10 (Rep1 rec), GTabulate10 (Rep1 rec))
      => Representable10 (Wrapped1 Generic1 rec) where
  type Rep10 (Wrapped1 Generic1 rec) = Field10 rec
  index10 (Wrapped1 rec) (Field10 f) = f rec
  tabulate10 f =
    Wrapped1 $ to1 $ gtabulate10 $ \i -> f $ Field10 $ getField10 i . from1
