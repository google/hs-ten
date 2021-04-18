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

-- | Provides an analog of 'Functor' over arity-1 type constructors.

{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Functor
         ( Functor10(..), (<$!), (<$>!), void10
         ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Generics
         ( Generic1(..)
         , (:.:)(..), (:*:)(..), (:+:)(..)
         , M1(..), Rec1(..), U1(..), V1, K1(..)
         )

import Data.Wrapped (Wrapped1(..))

-- | 'Functor' over arity-1 type constructors.
--
-- Whereas 'Functor' maps @a :: Type@ values to @b :: Type@ values, 'Functor10'
-- maps @(m :: k -> Type) a@ values to @m b@ values, parametrically in @a@.
-- That is, the type parameter of 'Functor' has arity 0, and the type
-- parameter of 'Functor10' has arity 1.
class Functor10 (f :: (k -> Type) -> Type) where
  -- | Map each @m a@ value in @f m@ parametrically to @n a@ to get @f m@.
  fmap10 :: (forall a. m a -> n a) -> f m -> f n

instance (Generic1 f, Functor10 (Rep1 f))
      => Functor10 (Wrapped1 Generic1 f) where
  fmap10 f = Wrapped1 . to1 . fmap10 f . from1 . unWrapped1

instance Functor10 (K1 i a) where
  fmap10 _ (K1 x) = K1 x

instance Functor10 V1 where
  fmap10 _ x = case x of {}

instance Functor10 U1 where
  fmap10 _ U1 = U1

deriving instance Functor10 f => Functor10 (Rec1 f)
deriving instance Functor10 f => Functor10 (M1 i c f)

instance (Functor10 f, Functor10 g) => Functor10 (f :+: g) where
  fmap10 f (L1 x) = L1 (fmap10 f x)
  fmap10 f (R1 x) = R1 (fmap10 f x)

instance (Functor10 f, Functor10 g) => Functor10 (f :*: g) where
  fmap10 f (l :*: r) = fmap10 f l :*: fmap10 f r

instance (Functor f, Functor10 g) => Functor10 (f :.: g) where
  fmap10 f (Comp1 x) = Comp1 $ fmap (fmap10 f) x

infixl 4 <$!
-- | ('<$') for 'Functor10'.
(<$!) :: Functor10 f => (forall a. n a) -> f m -> f n
x <$! f = fmap10 (const x) f

infixl 4 <$>!
-- | ('<$>') for 'Functor10'.
(<$>!) :: Functor10 f => (forall a. m a -> n a) -> f m -> f n
(<$>!) = fmap10

-- | 'Data.Functor.void' for 'Functor10'.
--
-- This returns @f 'Proxy'@ because @Proxy :: k -> Type@ has the right kind and
-- carries no runtime information.  It's isomorphic to @Const ()@ but easier to
-- spell.
void10 :: Functor10 f => f m -> f Proxy
void10 = fmap10 (const Proxy)
