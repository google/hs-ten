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

-- | Typeclass hierarchy of functors from @k -> Type@ to @Type@.
--
-- The naming convention @Functor10@ comes from the fact that it's a functor
-- from the category of objects with one type parameter to the category of
-- objects with zero type parameters.  See
-- <http://hackage.haskell.org/package/hakaru-0.4.0/docs/src/Language.Hakaru.Syntax.IClasses.html>
-- for precedent for this naming convention.
--
-- In this, the argument morphisms are of the form @forall a. m a -> n a@, and
-- the result morphisms are of the form @f m -> f n@.
--
-- The main parts of this are:
--
-- * 'Functor10' and the other similarly-named typeclasses, which are
-- essentially just translations of 'Functor' et al. to kind
-- @(k -> Type) -> Type@.
-- These are essentially 'Functor's with additional \"tag\" types
-- available at each occurrence of the argument.  Applying them to
-- @Identity@, you get essentially normal records, but other type parameters
-- give other interesting objects.
--
-- * (':**') and 'Exists' (two stock 'Functor10' types) plus appropriate
-- instances for products, sums, and compositions of functors as provided by
-- "GHC.Generics": (':*:'), (':+:'), and (':.:').
--
-- * 'Entails', which uses a GADT-like value to retrieve instances for its type
-- parameter.  This adds a lot of power to the otherwise-pretty-weak
-- 'Functor10' class hierarchy, since without access to corresponding
-- instances, all the input morphisms are unable to do anything whatsoever with
-- the \"tag\" types.  With 'Entails', though, one can use the fact that every
-- occurrence of @m a@ in @f m@ satisfies @c a@ to make instances of @c@
-- available while mapping/folding/traversing/etc. an @f m@.
--
-- The provided GHC.Generics-based deriving functionality is meant to be used
-- with the DerivingVia extension.  To get the full suite of classes on a
-- generic-record type, make sure every field is either an 'Ap10', another
-- nested generic-record, or an instance of 'Data.Functor.Update' applied to
-- one of the above.  Then, just add the deriving clauses:
--
-- @
--     data MyType f = MyType { _mrInt :: Ap10 Int f, _mrBool :: Ap10 Bool f }
--       deriving Generic1
--       deriving
--         ( Functor10, Foldable10, Traversable10
--         , Applicative10, Representable10, Update10, Constrained10 c
--         ) via Wrapped1 Generic1 MyType
--       deriving
--         ( Functor10WithIndex, Foldable10WithIndex, Traversable10WithIndex
--         ) via Wrapped1 Representable10 MyType
--
--     type instance Index10 MyType = Rep10 MyType
-- @

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten
  ( -- * Typeclasses
    -- ** Functor10
    module Data.Ten.Functor
  , module Data.Ten.Functor.WithIndex
    -- ** Foldable10
  , module Data.Ten.Foldable
  , module Data.Ten.Foldable.WithIndex
    -- ** Traversable10
  , module Data.Ten.Traversable
  , module Data.Ten.Traversable.WithIndex
    -- ** Applicative10
  , module Data.Ten.Applicative
    -- ** Representable10
  , module Data.Ten.Representable
    -- ** Update10
  , module Data.Ten.Update
    -- ** Entails
  , module Data.Ten.Entails

    -- * Standard 'Functor10's
  , module Data.Ten.Ap
  , module Data.Ten.Exists
  , module Data.Ten.Field
  , module Data.Ten.Sigma
  , (:.:)(..), (:*:)(..), (:+:)(..)
  ) where

import GHC.Generics ((:.:)(..), (:*:)(..), (:+:)(..))

import Data.Ten.Ap
import Data.Ten.Applicative
import Data.Ten.Entails
import Data.Ten.Exists
import Data.Ten.Field
import Data.Ten.Foldable
import Data.Ten.Foldable.WithIndex
import Data.Ten.Sigma
import Data.Ten.Functor
import Data.Ten.Functor.WithIndex
import Data.Ten.Representable
import Data.Ten.Traversable
import Data.Ten.Traversable.WithIndex
import Data.Ten.Update

-- TODO(awpr):
--
-- class Antimonoidal10 f where -- (Alternative10)
--   nah10 :: f (Const Void)
--   alt10 :: f m -> f m -> f (Sum m n)
--
-- class Contravariant10 f where
--   contramap10 :: (forall a. n a -> m a) -> f m -> f n
