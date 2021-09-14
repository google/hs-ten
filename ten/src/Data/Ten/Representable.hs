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

-- | Provides an analog of @Representable@ over arity-1 type constructors.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Representable
         ( Representable10(..)
         , rep10', field10'
         , distributeRep10, collectRep10
         , GTabulate10(..)
         , index10C
         ) where

import Data.Coerce (coerce)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
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
import Data.Ten.Entails (Entails(..), withEntailment)
import Data.Ten.Field (Field10(..))
import Data.Ten.Functor (Functor10(..))
import Data.Ten.Functor.WithIndex (Index10, Functor10WithIndex(..))
import Data.Ten.Foldable (Foldable10(..), fold10)
import Data.Ten.Foldable.WithIndex (Foldable10WithIndex(..))
import Data.Ten.Internal (starFst, starSnd)
import Data.Ten.Traversable (Traversable10(..))
import Data.Ten.Traversable.WithIndex (Traversable10WithIndex(..))

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

-- | Turn a record field selector targeting 'Ap10' into a 'Rep10'.
--
-- See also 'Data.Ten.Lens.rep10'.
field10'
  :: Representable10 rec
  => (rec (Rep10 rec) -> Ap10 a (Rep10 rec)) -> Rep10 rec a
field10' f = rep10' (unAp10 . f)

-- | Superclass appeasement; deriving via this will give infinite loops; don't!
instance Functor10 f => Functor10 (Wrapped1 Representable10 f) where
  fmap10 f (Wrapped1 fm) = Wrapped1 (fmap10 f fm)

-- | Superclass appeasement; deriving via this will give infinite loops; don't!
instance Foldable10 f => Foldable10 (Wrapped1 Representable10 f) where
  foldMap10 f (Wrapped1 fm) = foldMap10 f fm

-- | Superclass appeasement; deriving via this will give infinite loops; don't!
instance Traversable10 f => Traversable10 (Wrapped1 Representable10 f) where
  mapTraverse10 r f (Wrapped1 fm) = mapTraverse10 (r . Wrapped1) f fm

type instance Index10 (Wrapped1 Representable10 f) = Rep10 f

instance Representable10 f
      => Functor10WithIndex (Wrapped1 Representable10 f) where
  imap10 f (Wrapped1 fm) = Wrapped1 $ tabulate10 (\i -> f i (fm `index10` i))

instance (Representable10 f, Foldable10 f)
      => Foldable10WithIndex (Wrapped1 Representable10 f) where
  ifoldMap10 f fm = fold10 $ imap10 (Const .: f) fm

instance (Representable10 f, Traversable10 f)
      => Traversable10WithIndex (Wrapped1 Representable10 f) where
  imapTraverse10 r f fm = mapTraverse10 r unComp1 $ imap10 (Comp1 .: f) fm

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

-- | The 'Generic1' implementation of 'tabulate10' based on 'Field10'.
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

-- | Access an element along with an instance for its type parameter.
index10C
  :: forall c f a r m
   . (Representable10 f, Entails (Rep10 f) c)
  => f m -> Rep10 f a -> (c a => m a -> r) -> r
index10C fm k f = withEntailment @c k $ f (index10 fm k)
