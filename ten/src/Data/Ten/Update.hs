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

-- | Extends 'Representable10' with support for modifying elements.

{-# LANGUAGE PolyKinds #-}

module Data.Ten.Update (Update10(..), ix10) where

import Data.Functor ((<&>))
import Data.Kind (Type)

import Data.Ten.Representable (Representable10(..))

class Representable10 f => Update10 (f :: (k -> Type) -> Type) where
  -- | Modify an @f m@ at a given index.
  over10 :: Rep10 f a -> (m a -> m a) -> f m -> f m

-- | Update an @f m@ at a given index.
update10 :: Update10 f => Rep10 f a -> m a -> f m -> f m
update10 i = over10 i . const

-- | A 'Control.Lens.Lens' to the field identified by a given 'Rep10'.
--
-- @
--     ix10 :: Update10 f => Rep10 f a -> Lens' (f m) (m a)
-- @
ix10
  :: (Update10 f, Functor g)
  => Rep10 f a -> (m a -> g (m a)) -> f m -> g (f m)
ix10 i f = \fm -> f (index10 fm i) <&> \fma -> update10 i fma fm
