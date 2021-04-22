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

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Ten.Functor.WithIndex (Index10, Functor10WithIndex(..)) where

import GHC.Generics ((:.:)(..))

import Data.Ten.Functor (Functor10(..))

type family Index10 (f :: (k -> *) -> *) :: k -> *

type instance Index10 (g :.: f) = Index10 f

class Functor10 f => Functor10WithIndex f where
  imap10 :: (forall a. Index10 f a -> m a -> n a) -> f m -> f n

instance (Functor g, Functor10WithIndex f) => Functor10WithIndex (g :.: f) where
  imap10 f (Comp1 gfm) = Comp1 $ fmap (imap10 f) gfm
