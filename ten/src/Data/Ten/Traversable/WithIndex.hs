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

-- | An extension of 'Traversable10' that provides access to some 'Index10'.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Ten.Traversable.WithIndex
         ( Index10, Traversable10WithIndex(..), itraverse10
         , traverse10C
         ) where

import GHC.Generics ((:.:)(..))

import Data.Ten.Entails (Entails(..), byEntailment)
import Data.Ten.Foldable.WithIndex (Foldable10WithIndex(..))
import Data.Ten.Functor.WithIndex (Index10, Functor10WithIndex(..))
import Data.Ten.Traversable (Traversable10(..))

-- | An extension of 'Traversable10' that provides access to some 'Index10'.
class (Functor10WithIndex f, Foldable10WithIndex f, Traversable10 f)
   => Traversable10WithIndex f where
  imapTraverse10
    :: Applicative g
    => (f n -> r)
    -> (forall a. Index10 f a -> m a -> g (n a))
    -> f m -> g r

instance (Traversable g, Traversable10WithIndex f)
      => Traversable10WithIndex (g :.: f) where
  imapTraverse10 r f (Comp1 gfm) =
    r . Comp1 <$> traverse (imapTraverse10 id f) gfm

-- | 'Data.Ten.Traversable.traverse10' with an index parameter.
itraverse10
  :: (Applicative g, Traversable10WithIndex f)
  => (forall a. Index10 f a -> m a -> g (n a))
  -> f m -> g (f n)
itraverse10 = imapTraverse10 id

-- | 'Data.Ten.Traversable.traverse10' with an instance for every element.
traverse10C
  :: forall c f g m n
   . (Entails (Index10 f) c, Applicative g, Traversable10WithIndex f)
  => (forall a. c a => m a -> g (n a)) -> f m -> g (f n)
traverse10C f = itraverse10 (byEntailment @c f)
