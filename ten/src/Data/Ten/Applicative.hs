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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Applicative
         ( Applicative10(..), (<*!), (*>!)
         , liftA310
         , Arr10(..)
         ) where

import Control.Applicative (liftA2)
import GHC.Generics
         ( Generic1(..)
         , (:.:)(..), (:*:)(..)
         , M1(..), Rec1(..), U1(..)
         )

import Data.Wrapped (Wrapped1(..))

import Data.Ten.Functor (Functor10, (<$>!))

infixl 4 <*>!
-- | Analogous to 'Applicative'.
class Functor10 f => Applicative10 f where
  {-# MINIMAL pure10, ((<*>!) | liftA210) #-}

  pure10 :: (forall a. m a) -> f m

  (<*>!) :: f (Arr10 m n) -> f m -> f n
  (<*>!) = liftA210 (\ (Arr10 f') x' -> f' x')

  liftA210 :: (forall a. m a -> n a -> o a) -> f m -> f n -> f o
  liftA210 f x y = (Arr10 . f) <$>! x <*>! y

instance (Generic1 f, Applicative10 (Rep1 f))
      => Applicative10 (Wrapped1 Generic1 f) where
  pure10 x = Wrapped1 $ to1 $ pure10 x
  liftA210 f (Wrapped1 x) (Wrapped1 y) =
    Wrapped1 $ to1 $ liftA210 f (from1 x) (from1 y)

-- no instance Applicative10 (K1 i a): K1 is extra non-wrapped data
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

newtype Arr10 m n a = Arr10 { runArr10 :: m a -> n a }

liftA310 :: Applicative10 f
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


