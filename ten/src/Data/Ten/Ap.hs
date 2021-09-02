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

-- | Provides a field wrapper type to make @Generic1@ work with @Functor10@ etc.
--
-- GHC can't derive 'GHC.Generics.Generic1' instances for types that apply
-- their type parameter to a constant type (e.g. @data Thing f = Thing (f
-- Int)@, but it can handle the equivalent type when the application is hidden
-- under a newtype: @data Thing f = Thing (Ap10 Int f)@.  So, by wrapping each
-- field in this newtype and providing the appropriate instances, we can use
-- Generics to derive instances for the whole hierarchy of
-- 'Data.Ten.Functor.Functor10' and related classes.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Ap
         ( -- * Field Wrapper
           Ap10(..)
           -- * Instances
           -- $ap10_instances
         ) where

import Data.Kind (Constraint, Type)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Data.Default.Class (Default(..))
import Data.Hashable (Hashable(..))
import Data.Portray (Portray(..))
import Data.Portray.Diff (Diff)
import Data.Wrapped (Wrapped(..))

-- | A 'Data.Ten.Functor.Functor10' made by applying the argument to some type.
newtype Ap10 (a :: k) (f :: k -> Type) = Ap10 { unAp10 :: f a }
  deriving Generic

-- $ap10_instances
--
-- [Note: Ap10 instances]
--
-- Since @Ap10 a f@ is a newtype over @f a@, it can adopt any instance that
-- @f a@ has, e.g. @Eq (f a) => Eq (Ap10 a f)@.  This doesn't play very nicely
-- with inference of derived instance contexts, though: if you say @deriving
-- Eq@ on a type with an @f@ type parameter with an @Ap10 T f@ field, GHC will
-- complain about the missing instance @Eq (f T)@ rather than adding it to the
-- context.  However, if we can arrange for this to be expressed as a
-- Haskell98-looking constraint of the form @C f@, GHC will be willing to add
-- that to the inferred context.
--
-- We can do this by adding a new class @EqAp f@ with the instance we really
-- want as a superclass, and using that as the context of 'Ap10'\'s @Eq@
-- instance.  Now when trying to solve @Eq (Ap10 T f)@, GHC will simplify to
-- @(EqAp f, EqCtx f T)@.  However, if we have just a catch-all instance for
-- @EqAp@, GHC will simplify it further to the instance context of that
-- instance, which would bring us back to a constraint GHC won't add to the
-- context, @forall a. Eq a => Eq (f a)@.  We have to prevent GHC from doing
-- that simplification, which we can achieve by overlapping it with some other
-- instance, so that GHC can't choose the catch-all instance without knowing
-- more about @f@.  To avoid weird behavior from the overlap, we make an
-- otherwise-unused type 'Decoy' to carry the instance.
--
-- Finally, because @Ap10@ is poly-kinded, if we used @Eq@ directly as the
-- context of that quantified constraint, we'd be saying that @Ap10@ can only
-- be @Eq@ when its hidden kind parameter is @Type@.  Instead, we generalize it
-- to an associated type family 'EqCtx'.  This might be e.g.
-- 'GHC.TypeNats.KnownNat' for 'GHC.TypeNats.Nat's, or simply nothing for
-- phantom type parameters.  I'm not yet sure how to approach the instances for
-- other kinds -- for instance, should we provide stock ones, or expect users
-- to write kind-level newtypes and provide their own instances?
--
-- This trickery is applied to all the instances of Ap10.  In particular this
-- means @deriving (Eq, Ord, Read, Show, Default, NFData)@ and
-- @deriving (Portray, Diff) via Wrapped Generic T@ will all work.

newtype Decoy a = Decoy ()
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (Default, NFData, Hashable)
  deriving (Diff, Portray) via Wrapped Generic (Decoy a)

-- See [Note: Ap10 instances]
class (forall a. PortrayCtx f a => Portray (f a))
   => PortrayAp (f :: k -> Type) where
  type PortrayCtx f :: k -> Constraint

instance (forall a. Portray a => Portray (f a)) => PortrayAp f where
  type PortrayCtx f = Portray

instance {-# OVERLAPS #-} PortrayAp (Decoy :: Type -> Type) where
  type PortrayCtx Decoy = Portray

deriving newtype instance (PortrayCtx f a, PortrayAp f) => Portray (Ap10 a f)

-- See [Note: Ap10 instances]
class (forall a. DiffCtx f a => Diff (f a)) => DiffAp (f :: k -> Type) where
  type DiffCtx f :: k -> Constraint

instance (forall a. Diff a => Diff (f a)) => DiffAp f where
  type DiffCtx f = Diff

instance {-# OVERLAPS #-} DiffAp (Decoy :: Type -> Type) where
  type DiffCtx Decoy = Diff

deriving newtype instance (DiffCtx f a, DiffAp f) => Diff (Ap10 a f)

-- See [Note: Ap10 instances]
class (forall a. EqCtx f a => Eq (f a)) => EqAp (f :: k -> Type) where
  type EqCtx f :: k -> Constraint

instance (forall a. Eq a => Eq (f a)) => EqAp f where
  type EqCtx f = Eq

instance {-# OVERLAPS #-} EqAp (Decoy :: Type -> Type) where
  type EqCtx Decoy = Eq

deriving newtype instance (EqCtx f a, EqAp f) => Eq (Ap10 a f)

-- See [Note: Ap10 instances]
class (forall a. OrdCtx f a => Ord (f a)) => OrdAp (f :: k -> Type) where
  type OrdCtx f :: k -> Constraint

instance (forall a. Ord a => Ord (f a)) => OrdAp f where
  type OrdCtx f = Ord

instance {-# OVERLAPS #-} OrdAp (Decoy :: Type -> Type) where
  type OrdCtx Decoy = Ord

deriving newtype
  instance (OrdCtx f a, OrdAp f, EqCtx f a, EqAp f) => Ord (Ap10 a f)

-- See [Note: Ap10 instances]
class (forall a. ReadCtx f a => Read (f a)) => ReadAp (f :: k -> Type) where
  type ReadCtx f :: k -> Constraint

instance (forall a. Read a => Read (f a)) => ReadAp f where
  type ReadCtx f = Read

instance {-# OVERLAPS #-} ReadAp (Decoy :: Type -> Type) where
  type ReadCtx Decoy = Read

deriving newtype instance (ReadCtx f a, ReadAp f) => Read (Ap10 a f)

-- See [Note: Ap10 instances]
class (forall a. ShowCtx f a => Show (f a)) => ShowAp (f :: k -> Type) where
  type ShowCtx f :: k -> Constraint

instance (forall a. Show a => Show (f a)) => ShowAp f where
  type ShowCtx f = Show

instance {-# OVERLAPS #-} ShowAp (Decoy :: Type -> Type) where
  type ShowCtx Decoy = Show

deriving newtype instance (ShowCtx f a, ShowAp f) => Show (Ap10 a f)

-- See [Note: Ap10 instances]
class (forall a. DefaultCtx f a => Default (f a)) => DefaultAp (f :: k -> Type) where
  type DefaultCtx f :: k -> Constraint

instance (forall a. Default a => Default (f a)) => DefaultAp f where
  type DefaultCtx f = Default

instance {-# OVERLAPS #-} DefaultAp (Decoy :: Type -> Type) where
  type DefaultCtx Decoy = Default

deriving newtype instance (DefaultCtx f a, DefaultAp f) => Default (Ap10 a f)

-- See [Note: Ap10 instances]
class (forall a. NFDataCtx f a => NFData (f a)) => NFDataAp (f :: k -> Type) where
  type NFDataCtx f :: k -> Constraint

instance (forall a. NFData a => NFData (f a)) => NFDataAp f where
  type NFDataCtx f = NFData

instance {-# OVERLAPS #-} NFDataAp (Decoy :: Type -> Type) where
  type NFDataCtx Decoy = NFData

deriving newtype instance (NFDataCtx f a, NFDataAp f) => NFData (Ap10 a f)

-- See [Note: Ap10 instances]
class (forall a. HashableCtx f a => Hashable (f a)) => HashableAp (f :: k -> Type) where
  type HashableCtx f :: k -> Constraint

instance (forall a. Hashable a => Hashable (f a)) => HashableAp f where
  type HashableCtx f = Hashable

instance {-# OVERLAPS #-} HashableAp (Decoy :: Type -> Type) where
  type HashableCtx Decoy = Hashable

deriving newtype instance (HashableCtx f a, HashableAp f) => Hashable (Ap10 a f)
