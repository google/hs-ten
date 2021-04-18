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

-- | Provides an analog of 'Traversable' over arity-1 type constructors.

{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Traversable
         ( Traversable10(..), traverse10, sequenceA10
         ) where

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics
         ( Generic1(..)
         , (:.:)(..), (:*:)(..), (:+:)(..)
         , M1(..), Rec1(..), U1(..), V1, K1(..)
         )

import Data.Wrapped (Wrapped1(..))

import Data.Ten.Foldable (Foldable10)
import Data.Ten.Functor (Functor10)

(.:) :: (q -> r) -> (a -> b -> q) -> a -> b -> r
(.:) = (.) . (.)

-- | Analog of 'Traversable' over arity-1 type constructors.
--
-- This is defined in terms of 'mapTraverse10' for two reasons:
--
-- * First, it makes it possible to use with GeneralizedNewtypeDeriving and
--   DerivingVia.  See
--   https://ryanglscott.github.io/2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/
--   for more details.
-- * Second, it uses fewer 'fmap's in some cases: when you need to re-apply a
--   constructor tag like 'L1' or 'R1' after calling 'traverse10' on the
--   payload, this would normally be an additional 'fmap', but with
--   'mapTraverse10' it can be fused into the underlying recursive call.  Less
--   crucially, the same trick applies when traversing multiple fields and
--   combining them back into a product type: the first call can use
--   'mapTraverse10' to pre-apply the function, and use '<*>' rather than
--   'Control.Applicative.liftA2' (which is often defined as an 'fmap' followed
--   by a '<*>').
class (Functor10 t, Foldable10 t)
   => Traversable10 (t :: (k -> Type) -> Type) where
  -- | 'traverse10' with a built-in 'fmap' on the final result.
  mapTraverse10
    :: forall f m n r
     . Applicative f
    => (t n -> r)
    -> (forall a. m a -> f (n a))
    -> t m -> f r

-- | Analog of 'traverse' for functors over arity-1 type constructors.
--
-- Given a parametric function that takes the wrapped type @m a@ to @n a@ in an
-- 'Applicative' @f@, visit all contained @m _@s to convert from @t m@ to @t n@.
--
-- @m@ and @n@ here play the role of @a@ and @b@ in the normal 'traverse' type;
-- that is, instead of traversing to change a @Type@, we're traversing to change
-- a type constructor of kind @k -> Type@:
--
-- @
--     traverse
--       :: (Traversable t, Applicative f)
--       => (          a   -> f  b   ) -> t a -> f (t b)
--     traverse10
--       :: (Traversable10 t, Applicative f)
--       => (forall x. m x -> f (n x)) -> t m -> f (t n)
-- @
traverse10
  :: forall t f m n
   . (Traversable10 t, Applicative f)
  => (forall a. m a -> f (n a))
  -> t m -> f (t n)
traverse10 = mapTraverse10 id

instance (Generic1 f, Traversable10 (Rep1 f))
      => Traversable10 (Wrapped1 Generic1 f) where
  mapTraverse10 r f = mapTraverse10 (r . Wrapped1 . to1) f . from1 . unWrapped1

instance Traversable10 (K1 i a) where
  mapTraverse10 r _ k = pure (r $ coerce k)

instance Traversable10 V1 where
  mapTraverse10 _ _ x = case x of {}

instance Traversable10 U1 where
  mapTraverse10 r _ U1 = pure (r U1)

instance Traversable10 f => Traversable10 (Rec1 f) where
  mapTraverse10 r f (Rec1 x) = mapTraverse10 (r . Rec1) f x

instance Traversable10 f => Traversable10 (M1 i c f) where
  mapTraverse10 r f (M1 x) = mapTraverse10 (r . M1) f x

instance (Traversable10 f, Traversable10 g) => Traversable10 (f :+: g) where
  mapTraverse10 r f (L1 x) = mapTraverse10 (r . L1) f x
  mapTraverse10 r f (R1 x) = mapTraverse10 (r . R1) f x

instance (Traversable10 f, Traversable10 g) => Traversable10 (f :*: g) where
  mapTraverse10 r f (x :*: y) =
    mapTraverse10 (r .: (:*:)) f x <*> traverse10 f y

instance (Traversable f, Traversable10 g) => Traversable10 (f :.: g) where
  mapTraverse10 r f (Comp1 x) = r . Comp1 <$> traverse (traverse10 f) x

-- | 'sequenceA' for 'Traversable10'.
sequenceA10
  :: (Applicative f, Traversable10 t)
  => t (f :.: g) -> f (t g)
sequenceA10 = traverse10 coerce
