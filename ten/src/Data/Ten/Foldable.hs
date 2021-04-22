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

-- | Provides an analog of 'Foldable' over arity-1 type constructors.

{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Foldable
         ( Foldable10(..)
         , fold10, foldr10, foldl10, traverse10_, sequenceA10_, fsequenceA10_
         ) where

import Data.Functor (void)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.Monoid (Dual(..), Endo(..))
import GHC.Generics
         ( Generic1(..)
         , (:.:)(..), (:*:)(..), (:+:)(..)
         , M1(..), Rec1(..), U1(..), V1, K1(..)
         )

import Data.Wrapped (Wrapped1(..))

import Data.Ten.Ap (Ap10(..))

-- | 'Foldable' over arity-1 type constructors.
--
-- Whereas 'Foldable' folds @a :: Type@ values to a monoid, 'Foldable10' folds
-- @(m :: k -> Type) a@ values to a monoid, parametrically in @a@.  That is,
-- the type parameter of 'Foldable' has arity 0, and the type parameter of
-- 'Foldable10' has arity 1.
class Foldable10 (t :: (k -> Type) -> Type) where
  -- | Map each @m a@ element parametrically to @w@ and 'mconcat' the results.
  foldMap10 :: Monoid w => (forall a. m a -> w) -> t m -> w

instance (Generic1 f, Foldable10 (Rep1 f))
      => Foldable10 (Wrapped1 Generic1 f) where
  foldMap10 f = foldMap10 f . from1 . unWrapped1

instance Foldable10 (Ap10 a) where
  foldMap10 f (Ap10 x) = f x

instance Foldable10 (K1 i a) where
  foldMap10 _ (K1 _) = mempty

instance Foldable10 V1 where
  foldMap10 _ x = case x of {}

instance Foldable10 U1 where
  foldMap10 _ U1 = mempty

deriving instance Foldable10 f => Foldable10 (Rec1 f)
deriving instance Foldable10 f => Foldable10 (M1 i c f)

instance (Foldable10 f, Foldable10 g) => Foldable10 (f :+: g) where
  foldMap10 f (L1 x) = foldMap10 f x
  foldMap10 f (R1 x) = foldMap10 f x

instance (Foldable10 f, Foldable10 g) => Foldable10 (f :*: g) where
  foldMap10 f (l :*: r) = foldMap10 f l <> foldMap10 f r

instance (Foldable f, Foldable10 g) => Foldable10 (f :.: g) where
  foldMap10 f (Comp1 x) = foldMap (foldMap10 f) x

-- | Given a structure over @'Const' m@, return the ('<>') of all elements.
fold10 :: (Foldable10 t, Monoid m) => t (Const m) -> m
fold10 = foldMap10 getConst

-- | Right-associative fold over a 'Foldable10'.
foldr10 :: Foldable10 t => (forall a. m a -> b -> b) -> b -> t m -> b
foldr10 f z = flip appEndo z . foldMap10 (Endo . f)

-- | Left-associative fold over a 'Foldable10'.
foldl10 :: Foldable10 t => (forall a. b -> m a -> b) -> b -> t m -> b
foldl10 f z = flip appEndo z . getDual . foldMap10 (Dual . Endo . flip f)

-- | Sequence actions given by a function left-to-right in a 'Foldable10'.
--
-- This form discards the final result; see 'Data.Ten.Traversable.traverse10'
-- for a version that keeps it.
traverse10_
  :: (Applicative f, Foldable10 t) => (forall a. m a -> f ()) -> t m -> f ()
traverse10_ f = foldl10 (\a x -> a <* f x) (pure ())

-- | Sequence actions in a 'Foldable10' left-to-right, discarding the result.
--
-- This variant expects the composition of the 'Applicative' being sequenced
-- with some inner type constructor at each field.
--
-- See 'Data.Ten.Traversable.fsequenceA10_' for a version that keeps the result.
fsequenceA10_ :: (Applicative m, Foldable10 f) => f (m :.: n) -> m ()
fsequenceA10_ = traverse10_ (void . unComp1)

-- | Sequence actions in a 'Foldable10' left-to-right, discarding the result.
--
-- This variant expects just the plain @m@ actions with no inner type
-- constructor.
sequenceA10_ :: (Applicative m, Foldable10 f) => f m -> m ()
sequenceA10_ = traverse10_ void
