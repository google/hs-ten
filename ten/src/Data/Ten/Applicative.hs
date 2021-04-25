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

-- | Provides an analog of 'Applicative' over arity-1 type constructors.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Applicative
         ( Applicative10(..), (<*!), (*>!)
         , liftA310
         , (:->:)(Arr10, runArr10)
         , pure10C, liftA210C, liftA310C
         ) where

import Control.Applicative (liftA2)
import Data.Proxy (Proxy(..))
import GHC.Generics
         ( Generic1(..)
         , (:.:)(..), (:*:)(..)
         , K1(..), M1(..), Rec1(..), U1(..)
         )

import Data.Wrapped (Wrapped1(..))

import Data.Ten.Ap (Ap10(..))
import Data.Ten.Entails (Entails)
import Data.Ten.Functor (Functor10, (<$>!))
import Data.Ten.Functor.WithIndex (Index10, Functor10WithIndex, fmap10C)

infixl 4 <*>!
-- | 'Applicative' over arity-1 type constructors.
--
-- See also 'Functor10' and 'Data.Ten.Foldable.Foldable10'.
class Functor10 f => Applicative10 f where
  {-# MINIMAL pure10, ((<*>!) | liftA210) #-}

  -- | Lift a parametric @m@ value into an @f m@.
  pure10 :: (forall a. m a) -> f m

  -- | ('<*>') for 'Applicative10': zip two @f@s with 'runArr10'.
  (<*>!) :: f (m :->: n) -> f m -> f n
  (<*>!) = liftA210 (\ (Arr10 f') x' -> f' x')

  -- | 'Control.Applicative.liftA2' for 'Applicative10': zip two @f@s with a
  -- parametric function.
  liftA210 :: (forall a. m a -> n a -> o a) -> f m -> f n -> f o
  liftA210 f x y = (Arr10 . f) <$>! x <*>! y

instance (Generic1 f, Applicative10 (Rep1 f))
      => Applicative10 (Wrapped1 Generic1 f) where
  pure10 x = Wrapped1 $ to1 $ pure10 x
  liftA210 f (Wrapped1 x) (Wrapped1 y) =
    Wrapped1 $ to1 $ liftA210 f (from1 x) (from1 y)

instance Applicative10 (Ap10 a) where
  pure10 x = Ap10 x
  liftA210 f (Ap10 x) (Ap10 y) = Ap10 $ f x y

instance Monoid a => Applicative10 (K1 i a) where
  pure10 _ = K1 mempty
  liftA210 _ (K1 x) (K1 y) = K1 (x <> y)

-- no instance Applicative10 V1: V1 is uninhabited

instance Applicative10 U1 where
  pure10 _ = U1
  liftA210 _ U1 U1 = U1

deriving instance Applicative10 f => Applicative10 (Rec1 f)
deriving instance Applicative10 f => Applicative10 (M1 i c f)

-- no instance (Applicative10 f, Applicative10 g) => Applicative10 (f :+: g)

instance (Applicative10 f, Applicative10 g) => Applicative10 (f :*: g) where
  pure10 x = pure10 x :*: pure10 x
  liftA210 f (xl :*: xr) (yl :*: yr) = liftA210 f xl yl :*: liftA210 f xr yr

instance (Applicative f, Applicative10 g) => Applicative10 (f :.: g) where
  pure10 x = Comp1 $ pure (pure10 x)
  liftA210 f (Comp1 x) (Comp1 y) = Comp1 $ liftA2 (liftA210 f) x y

-- | A function @m a -> n a@ wrapped in a newtype for use as a type parameter.
--
-- This is used to represent the partially-applied functions in the left side
-- of ('<*>!').
newtype (m :->: n) a = Arr10 { runArr10 :: m a -> n a }

-- | 'Control.Applicative.liftA3' for 'Applicative10'.
liftA310
  :: Applicative10 f
  => (forall a. m a -> n a -> o a -> p a) -> f m -> f n -> f o -> f p
liftA310 f xs ys zs =
  (\x -> Arr10 (Arr10 . f x)) <$>! xs <*>! ys <*>! zs

infixl 4 <*!
-- | ('<*') for 'Applicative10'.
(<*!) :: Applicative10 f => f m -> f n -> f m
(<*!) = liftA210 const

infixl 4 *>!
-- | ('*>') for 'Applicative10'.
(*>!) :: Applicative10 f => f m -> f n -> f n
(*>!) = liftA210 (const id)

-- | 'pure10' with access to an instance for every element.
pure10C
  :: forall c f m
   . (Entails (Index10 f) c, Applicative10 f, Functor10WithIndex f)
  => (forall a. c a => m a) -> f m
pure10C x = fmap10C @c (const x) (pure10 Proxy)

-- | 'liftA210' with access to an instance for every element.
liftA210C
  :: forall c f m n o
   . (Entails (Index10 f) c, Applicative10 f, Functor10WithIndex f)
  => (forall a. c a => m a -> n a -> o a)
  -> f m -> f n -> f o
liftA210C f x y = fmap10C @c (Arr10 . f) x <*>! y

-- | 'liftA310' with access to an instance for every element.
liftA310C
  :: forall c f m n o p
   . (Entails (Index10 f) c, Applicative10 f, Functor10WithIndex f)
  => (forall a. c a => m a -> n a -> o a -> p a)
  -> f m -> f n -> f o -> f p
liftA310C f x y z = liftA210C @c (fmap Arr10 . f) x y <*>! z
