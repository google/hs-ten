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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Data.Ten.Ap (Ap10(..)) where

import Data.Kind (Type)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Data.Default.Class (Default(..))
import Data.Portray (Portray)

-- | A 'Data.Ten.Functor.Functor10' made by applying the argument to some type.
newtype Ap10 (a :: k) (f :: k -> Type) = Ap10 { unAp10 :: f a }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Default, Portray)

instance (NFData a, NFData (f a)) => NFData (Ap10 a f)
