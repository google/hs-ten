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

-- | Recovers instances for the elements of a 'Functor10' or similar type.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Constrained
         ( Constrained(..), withConstrained
         , Constrained10(..)
         , fmap10C, (<$?), (<$>?)
         , foldMap10C, foldr10C, foldl10C, traverse10C_
         , traverse10C
         , pure10C, liftA210C, liftA310C
         , index10C
         ) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import GHC.Generics
         ( Generic1(..)
         , (:.:)(..), (:*:)(..), (:+:)(..)
         , M1(..), Rec1(..), U1(..), V1, K1(..)
         )

import Data.Portray (Portray(..))
import Data.Portray.Pretty (WrappedPortray(..))
import Data.Wrapped (Wrapped1(..))
import Text.PrettyPrint.HughesPJClass (Pretty)

import Data.Ten.Applicative (Applicative10(..), liftA210, liftA310)
import Data.Ten.Foldable (Foldable10, foldl10, foldr10, foldMap10, traverse10_)
import Data.Ten.Functor (Functor10(..))
import Data.Ten.Representable (Representable10(..))
import Data.Ten.Traversable (Traversable10, traverse10)

-- | Product of a constraint constructor and type constructor.
--
-- @Constrained cxt m a@ holds both an instance @cxt a@ and a value @m a@.
-- This is used to hold instances for the type parameters of each @m@ held in
-- an @f m@.
data Constrained (cxt :: k -> Constraint) (m :: k -> Type) (a :: k) where
  Constrained :: forall cxt m a. cxt a => m a -> Constrained cxt m a
  deriving Pretty via WrappedPortray (Constrained cxt m a)

instance Portray (m a) => Portray (Constrained cxt m a) where
  portray (Constrained x) = portray x

-- | Bring the instance held by a 'Constrained' into scope.
withConstrained :: (cxt a => m a -> r) -> Constrained cxt m a -> r
withConstrained f (Constrained x) = f x

-- | @Constrained10 cxt f@ means that in @f m@, all applications of @m@
-- are to types @x@ that satisfy constraint @cxt@.
--
-- By having both 'constrained10' and 'unconstrained10' here, this is usable by
-- both covariant and contravariant functors.  For covariant functors,
-- 'constrained10' tags each value with the corresponding instance, and
-- 'unconstrained10' drops the instances.  For contravariant functors, the
-- roles are effectively reversed -- if an @f (Constrained cxt m)@ has some
-- applications of @Constrained cxt m@ in negative position, 'unconstrained10'
-- will automatically provide those constraints and leave just applications of
-- @m@ in negative position.
class Constrained10 (cxt :: k -> Constraint) (f :: (k -> Type) -> Type) where
  -- | Recover instances of @cxt@ to accompany each @m@ element in @f@.
  constrained10 :: f m -> f (Constrained cxt m)
  -- TODO(awpr):
  -- default constrained10 :: Contravariant10 f => f m -> f (Constrained cxt m)
  -- constrained10 = contramap10 (\ (Constrained x) -> x)

  unconstrained10 :: f (Constrained cxt m) -> f m
  default unconstrained10 :: Functor10 f => f (Constrained cxt m) -> f m
  unconstrained10 = fmap10 (\ (Constrained x) -> x)

  {-# MINIMAL constrained10 #-}

instance (Generic1 f, Constrained10 cxt (Rep1 f))
      => Constrained10 cxt (Wrapped1 Generic1 f) where
  constrained10 = Wrapped1 . to1 . constrained10 . from1 . unWrapped1
  unconstrained10 = Wrapped1 . to1 . unconstrained10 . from1 . unWrapped1

instance Constrained10 cxt (K1 i a) where
  constrained10 (K1 x) = K1 x

instance Constrained10 cxt V1 where
  constrained10 x = case x of {}

instance Constrained10 cxt U1 where
  constrained10 U1 = U1

deriving instance Constrained10 cxt f => Constrained10 cxt (Rec1 f)
deriving instance Constrained10 cxt f => Constrained10 cxt (M1 i c f)

instance (Constrained10 cxt f, Constrained10 cxt g)
      => Constrained10 cxt (f :+: g) where
  constrained10 (L1 x) = L1 (constrained10 x)
  constrained10 (R1 x) = R1 (constrained10 x)

  unconstrained10 (L1 x) = L1 (unconstrained10 x)
  unconstrained10 (R1 x) = R1 (unconstrained10 x)

instance (Constrained10 cxt f, Constrained10 cxt g)
      => Constrained10 cxt (f :*: g) where
  constrained10 (l :*: r) = constrained10 l :*: constrained10 r
  unconstrained10 (l :*: r) = unconstrained10 l :*: unconstrained10 r

instance (Functor f, Constrained10 cxt g) => Constrained10 cxt (f :.: g) where
  constrained10 (Comp1 f) = Comp1 (fmap constrained10 f)
  unconstrained10 (Comp1 f) = Comp1 (fmap unconstrained10 f)

-- | 'fmap10' with access to an instance for every element.
fmap10C
  :: forall cxt f m n
   . (Constrained10 cxt f, Functor10 f)
  => (forall a. cxt a => m a -> n a) -> f m -> f n
fmap10C f = fmap10 (withConstrained @cxt f) . constrained10

-- | ('Data.Ten.Functor.<$!') with access to an instance for every element.
--
-- It might not be possible to use this as an operator because it needs
-- TypeApplications to determine @cxt@.
(<$?)
  :: forall cxt f m n
   . (Constrained10 cxt f, Functor10 f)
  => (forall a. n a) -> f m -> f n
(<$?) x = fmap10C @cxt (const x)

-- | ('Data.Ten.Functor.<$>!') with access to an instance for every element.
--
-- It might not be possible to use this as an operator because it needs
-- TypeApplications to determine @cxt@.
(<$>?)
  :: forall cxt f m n
   . (Constrained10 cxt f, Functor10 f)
  => (forall a. cxt a => m a -> n a) -> f m -> f n
(<$>?) = fmap10C @cxt

-- | 'foldMap10' with access to an instance for every element.
foldMap10C
  :: forall cxt t m w
   . (Constrained10 cxt t, Foldable10 t, Monoid w)
  => (forall a. cxt a => m a -> w) -> t m -> w
foldMap10C f = foldMap10 (withConstrained @cxt f) . constrained10

-- | 'foldr10' with access to an instance for every element.
foldr10C
  :: forall cxt t m b
   . (Constrained10 cxt t, Foldable10 t)
  => (forall a. cxt a => m a -> b -> b) -> b -> t m -> b
foldr10C f z = foldr10 (withConstrained @cxt f) z . constrained10

-- | 'foldl10' with access to an instance for every element.
foldl10C
  :: forall cxt t m b
   . (Constrained10 cxt t, Foldable10 t)
  => (forall a. cxt a => b -> m a -> b) -> b -> t m -> b
foldl10C f z = foldl10 (flip (withConstrained @cxt (flip f))) z . constrained10

-- | 'traverse10_' with access to an instance for every element.
traverse10C_
  :: forall cxt f t m n
   . (Constrained10 cxt t, Applicative f, Foldable10 t)
  => (forall a. cxt a => m a -> f (n a)) -> t m -> f ()
traverse10C_ f =
  --          ewwwwwwwwww
  traverse10_ @_ @_ @_ @n (withConstrained @cxt f) . constrained10

-- | 'traverse10' with access to an instance for every element.
traverse10C
  :: forall cxt f t m n
   . (Constrained10 cxt t, Applicative f, Traversable10 t)
  => (forall a. cxt a => m a -> f (n a)) -> t m -> f (t n)
traverse10C f = traverse10 (withConstrained @cxt f) . constrained10

-- | 'pure10' with access to an instance for every element.
pure10C
  :: forall cxt f m
   . (Constrained10 cxt f, Applicative10 f)
  => (forall a. cxt a => m a) -> f m
pure10C x = fmap10C @cxt (const x) (pure10 Proxy)

-- | 'liftA210' with access to an instance for every element.
liftA210C
  :: forall cxt f m n o
   . (Constrained10 cxt f, Applicative10 f)
  => (forall a. cxt a => m a -> n a -> o a)
  -> f m -> f n -> f o
liftA210C f = liftA210 (withConstrained @cxt f) . constrained10

-- | 'liftA310' with access to an instance for every element.
liftA310C
  :: forall cxt f m n o p
   . (Constrained10 cxt f, Applicative10 f)
  => (forall a. cxt a => m a -> n a -> o a -> p a)
  -> f m -> f n -> f o -> f p
liftA310C f = liftA310 (withConstrained @cxt f) . constrained10

-- | Access an element along with an instance for its type parameter.
index10C
  :: forall cxt f a r m
   . (Representable10 f, Constrained10 cxt f)
  => f m -> Rep10 f a -> (cxt a => m a -> r) -> r
index10C fm k f = withConstrained @cxt f $ index10 (constrained10 @_ @cxt fm) k

