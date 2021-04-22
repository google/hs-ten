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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | A typeclass of GADT-like types whose existence implies an instance.

module Data.Ten.Entails
         ( Entails(..), Dict1(..), (:!:)
         , withEntailment, byEntailment
         ) where

import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))
import Type.Reflection (TypeRep)

-- We could just use Dict from "constraints", but it'd be a dependency that we
-- don't really need to have.

-- | A dictionary for the given arity-1 constraint constructor ("a class").
data Dict1 (c :: k -> Constraint) a = c a => Dict1

-- | A typeclass of GADT-like types whose existence implies an instance.
--
-- For a trivial example, the existence of @Int :~: b@ implies @Show b@, since
-- we can discover that @b@ is @Int@ and use the @Show Int@ instance.
class Entails k c where
  entailment :: k a -> Dict1 c a

-- | Non-GADTy types don't entail anything except parametric instances.
instance (forall a. c a) => Entails TypeRep c where entailment _ = Dict1
instance (forall a. c a) => Entails Proxy c where entailment _ = Dict1

-- | Equality to a particular type entails any instance that type has.
instance c a => Entails ((:~:) a) c where entailment Refl = Dict1

-- | 'Dict1's entail their own type parameter.
instance Entails (Dict1 c) c where entailment = id

-- | A utility "typeclass newtype" that's convenient with 'Entails'.
--
-- If you want to use 'entailment' to get an instance of the form @c (d a)@,
-- use @entailment \@(c :!: d)@.  Really I wanted to use the name (@:.:@), but
-- it's taken, so I just replaced the period with something else that involves
-- a dot near the typographic base line.
class c (d a) => (c :!: d) a
instance c (d a) => (c :!: d) a

-- | Bring an instance into scope using an 'Entails' instance.
--
-- @
--     (\ (k :: Int :~: b) (x :: b) -> withEntailment @Show k (show x)) Refl 2
-- @
withEntailment :: forall c k a r. Entails k c => k a -> (c a => r) -> r
withEntailment k r = case entailment @_ @c k of Dict1 -> r

-- | @flip 'withEntailment'@.
--
-- This is useful for "consuming" an index off the front of a function type and
-- turning it into an instance, e.g. in the context of an
-- 'Data.Ten.Functor.WithIndex.imap10' call.
byEntailment :: forall c k a r. Entails k c => (c a => r) -> k a -> r
byEntailment r k = withEntailment @c k r
