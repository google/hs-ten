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
-- - 'Functor10' and the other similarly-named typeclasses, which are
-- essentially just translations of 'Functor' et al. to kind
-- @(k -> Type) -> Type@.
-- These are essentially 'Functor's with additional \"tag\" types
-- available at each occurrence of the argument.  Applying them to
-- @Identity@, you get essentially records, but other type parameters
-- give other interesting objects.
-- - 'Constrained10', which allows a 'Functor10' to provide instances of a
-- typeclass for each of its elements.  This adds a lot of power to the
-- otherwise-pretty-weak 'Functor10' class hierarchy, since without access to
-- corresponding instances, all the input morphisms are unable to do anything
-- whatsoever with the \"tag\" types.
--
-- The provided GHC.Generics-based deriving functionality is meant to be used
-- with the DerivingVia extension.  To get the full suite of classes on a
-- generic-record type, make sure every field is either an 'Ap10', another
-- nested generic-record, or an instance of 'Update' applied to one of the
-- above.  Then, just add the deriving clauses:
--
-- data MyType = MyType { ... }
--   deriving Generic1
--   deriving
--     ( Functor10, Foldable10, Traversable10
--     , Applicative10, Constrained10 cxt
--     ) via Wrapped1 Generic1 MyType

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
    -- ** Foldable10
  , module Data.Ten.Foldable
    -- ** Traversable10
  , module Data.Ten.Traversable
    -- ** Applicative10
  , module Data.Ten.Applicative
    -- ** Constrained10
  , module Data.Ten.Constrained
    -- ** Representable10
  , module Data.Ten.Representable
    -- ** Update10
  , module Data.Ten.Update

    -- * Standard 'Functor10's
  , module Data.Ten.Ap
  , module Data.Ten.Exists
  , module Data.Ten.Field
  , (:.:)(..), (:*:)(..), (:+:)(..)
  ) where

import GHC.Generics ((:.:)(..), (:*:)(..), (:+:)(..))

import Data.Ten.Ap
import Data.Ten.Applicative
import Data.Ten.Constrained
import Data.Ten.Exists
import Data.Ten.Field
import Data.Ten.Foldable
import Data.Ten.Functor
import Data.Ten.Representable
import Data.Ten.Traversable
import Data.Ten.Update

-- TODO(awpr):
--
-- class Antimonoidal10 f where -- (Alternative10)
--   nah10 :: f (Const Void)
--   alt10 :: f m -> f m -> f (Sum m n)
--
-- class Contravariant10 f where
--   contramap10 :: (forall a. n a -> m a) -> f m -> f n
