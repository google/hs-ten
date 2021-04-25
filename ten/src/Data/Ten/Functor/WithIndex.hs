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

-- | An extension of 'Functor10' that provides access to some 'Index10'.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Ten.Functor.WithIndex
         ( Index10
         , Functor10WithIndex(..), fmap10C
         ) where

import Data.Kind (Type)
import GHC.Generics ((:.:)(..))

import Data.Ten.Entails (Entails, byEntailment)
import Data.Ten.Functor (Functor10(..))

-- | The index type associated with a given @f@.
--
-- This is often a GADT-like type, in that inspecting @Index10 f a@ can refine
-- @a@ to some more concrete type, provide instances for it via 'Entails', etc.
type family Index10 (f :: (k -> Type) -> Type) :: k -> Type

type instance Index10 (g :.: f) = Index10 f

-- | An extension of 'Functor10' that provides access to some 'Index10'.
class Functor10 f => Functor10WithIndex f where
  imap10 :: (forall a. Index10 f a -> m a -> n a) -> f m -> f n

instance (Functor g, Functor10WithIndex f) => Functor10WithIndex (g :.: f) where
  imap10 f (Comp1 gfm) = Comp1 $ fmap (imap10 f) gfm

-- | 'fmap10' with access to an instance for every element.
fmap10C
  :: forall c f m n
   . (Entails (Index10 f) c, Functor10WithIndex f)
  => (forall a. c a => m a -> n a) -> f m -> f n
fmap10C f = imap10 (byEntailment @c f)
