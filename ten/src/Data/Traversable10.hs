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

-- | Typeclass hierarchy of functors from @k -> Type@ to @*@.
--
-- The naming convention @XYZ10@ comes from the fact that it's a functor from
-- the category of objects with one type parameter to the category of objects
-- with zero type parameters.  See
-- <http://hackage.haskell.org/package/hakaru-0.4.0/docs/src/Language.Hakaru.Syntax.IClasses.html>
-- for precedent for this naming convention.
--
-- In this, the argument morphisms are of the form @forall a. m a -> n a@, and
-- the result morphisms are of the form @f m -> f n@.
--
-- The main parts of this are:
-- - 'Functor10' and the other similarly-named typeclasses, which are
-- essentially just translations of 'Functor' et al. to kind
-- @(k -> Type) -> Type@.
-- These are essentially 'Functor's with additional \"tag\" types
-- available at each occurrence of the argument.  Applying them to
-- 'Identity', you get essentially records, but other type parameters
-- give other interesting objects.
-- - 'Constrained10', which allows a 'Functor10' to provide instances of a
-- typeclass for each of its elements.  This adds a lot of power to the
-- otherwise-pretty-weak 'Functor10' class hierarchy, since without access to
-- corresponding instances, all the input morphisms are unable to do anything
-- whatsoever with the \"tag\" types.
-- - \"Functor10 kit\", a bunch of obvious types that implement 'Functor10'.
--   - const\/sum\/product over \"tag\" types (names XYZ10)
--   - const\/sum\/product over modifier types (names XYZF10)
--   - composition\/sum\/product over 'Functor10's
--   - Some
--
-- The provided GHC.Generics-based deriving functionality is meant to be used
-- with the DerivingVia extension.  To get the full suite of classes on a
-- generic-record type, make sure every field is either an 'Ap10', another
-- nested generic-record, or a 'Record00' type applied to one of the above.
-- Then, just add the deriving clauses:
--
-- data MyType = MyType { ... }
--   deriving Generic1
--   deriving
--     ( Functor10, Foldable10, Traversable10
--     , Applicative10, Constrained10 cxt
--     ) via Wrapped1 Generic1 MyType
--
-- TODO(awpr): it'd be nice to resurrect the motivation text from
-- Traversable10/Record.hs here, since this module header is fairly spartan and
-- technical currently.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Traversable10
  ( -- * Typeclasses
    -- ** Functor10
    Functor10(..), (<$!), (<$>!), void10
    -- ** Foldable10
  , Foldable10(..), fold10, foldr10, foldl10, traverse10_, sequenceA10_
    -- ** Traversable10
  , Traversable10(..), traverse10, sequenceA10
  , Traversal10, LensLike10
    -- ** Applicative10
  , Applicative10(..), (<*!), (*>!)
  , liftA310
  , Arr10(..)
    -- ** Constrained10
  , Constrained(..), withConstrained
  , Constrained10(..)
  , fmap10C, (<$?), (<$>?)
  , foldMap10C, foldr10C, foldl10C, traverse10C_
  , traverse10C
  , pure10C, liftA210C, liftA310C
    -- ** Representable10
  , Representable10(..), index10C, ix10
  , imap10, ifoldMap10, ifoldl10, ifoldr10, itraverse10
  , rep10, rep10'
  , distributeRep10, collectRep10

    -- * GHC.Generics implementations

    -- * Standard 'Functor10's
    -- ** Over types
  , Ap10(..), ap10, Const10(..), Pair10(..), Maybe10(..), Either10(..)
    -- ** Over type constructors
  , ConstF10(..), PairF10(..), EitherF10(..), Exists(..)
    -- ** Over 'Functor10's
  , (:.:)(..), comp
  , ProductF10(..), SumF10(..)
  ) where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.Functor.Constant (Constant(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Monoid (Dual(..), Endo(..))
import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)
import GHC.Generics

import Control.Lens (Lens', Getting, Iso, iso, lens, view)
import Data.Default.Class (Default)
import Data.Portray (Portray(..), Portrayal(..))
import Data.Portray.Pretty (WrappedPortray(..))
import Data.Wrapped (Wrapped(..), Wrapped1(..))
import Text.PrettyPrint.HughesPJClass (Pretty)

(.:) :: (q -> r) -> (a -> b -> q) -> a -> b -> r
(.:) = (.) . (.)

-- | A functor from the category @* -> Type@ to the category @*@.
class Functor10 (f :: (k -> Type) -> Type) where
  fmap10 :: (forall a. m a -> n a) -> f m -> f n
  default fmap10 :: Traversable10 f => (forall a. m a -> n a) -> f m -> f n
  fmap10 f = runIdentity . traverse10 (Identity . f)

instance (Generic1 f, Functor10 (Rep1 f)) => Functor10 (Wrapped1 Generic1 f) where
  fmap10 f = Wrapped1 . to1 . fmap10 f . from1 . unWrapped1

instance Functor10 (K1 i a)
instance Functor10 V1
instance Functor10 U1

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

-- | 'void' for 'Functor10'.
--
-- This returns @f 'Proxy'@ because @Proxy :: k -> Type@ has the right kind and
-- carries no runtime information.  It's isomorphic to @Const ()@ but easier to
-- spell.
void10 :: Functor10 f => f m -> f Proxy
void10 = fmap10 (const Proxy)

type LensLike10 f s t m n = (forall a. m a -> f (n a)) -> s -> f t

-- TODO(awpr): What to do with these?
-- type Getter10 s m = forall f. (Functor f, Contravariant f) => LensLike10 f s s m m
-- type Fold10 s m = forall f. (Applicative f, Contravariant f) => LensLike10 f s s m m
-- type Setter10 s t m n = LensLike10 Identity s t m n
-- type Lens10 s t m n = forall f. Functor f => LensLike10 f s t m n
type Traversal10 s t m n = forall f. Applicative f => LensLike10 f s t m n

class Foldable10 (t :: (k -> Type) -> Type) where
  foldMap10 :: Monoid w => (forall a. m a -> w) -> t m -> w
  default foldMap10
    :: (Monoid w, Traversable10 t) => (forall a. m a -> w) -> t m -> w
  foldMap10 f = getConstant . traverse10 (Constant . f)

instance (Generic1 f, Foldable10 (Rep1 f))
      => Foldable10 (Wrapped1 Generic1 f) where
  foldMap10 f = foldMap10 f . from1 . unWrapped1

instance Foldable10 (K1 i a)
instance Foldable10 V1
instance Foldable10 U1

deriving instance Foldable10 f => Foldable10 (Rec1 f)
deriving instance Foldable10 f => Foldable10 (M1 i c f)

instance (Foldable10 f, Foldable10 g) => Foldable10 (f :+: g) where
  foldMap10 f (L1 x) = foldMap10 f x
  foldMap10 f (R1 x) = foldMap10 f x

instance (Foldable10 f, Foldable10 g) => Foldable10 (f :*: g) where
  foldMap10 f (l :*: r) = foldMap10 f l <> foldMap10 f r

instance (Foldable f, Foldable10 g) => Foldable10 (f :.: g) where
  foldMap10 f (Comp1 x) = foldMap (foldMap10 f) x

fold10 :: (Foldable10 t, Monoid m) => t (Constant m) -> m
fold10 = foldMap10 getConstant

foldr10 :: Foldable10 t => (forall a. m a -> b -> b) -> b -> t m -> b
foldr10 f z = flip appEndo z . foldMap10 (Endo . f)

foldl10 :: Foldable10 t => (forall a. b -> m a -> b) -> b -> t m -> b
foldl10 f z = flip appEndo z . getDual . foldMap10 (Dual . Endo . flip f)

traverse10_
  :: (Applicative f, Foldable10 t) => (forall a. m a -> f (n a)) -> t m -> f ()
traverse10_ f = foldl10 (\a x -> a <* f x) (pure ())

sequenceA10_ :: (Applicative f, Foldable10 t) => t (f :.: g) -> f ()
sequenceA10_ = traverse10_ unComp1

-- | Analog of 'Traversable' for @(k -> Type) -> Type@ functors.
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
--   'liftA2' (which is often defined as an 'fmap' followed by a '<*>').
class (Functor10 t, Foldable10 t) =>
    Traversable10 (t :: (k -> Type) -> Type) where
  mapTraverse10
    :: forall f m n r
     . Applicative f
    => (t n -> r)
    -> (forall a. m a -> f (n a))
    -> t m -> f r

-- | Analog of 'traverse' for @(k -> Type) -> Type@ functors.
--
-- Given a function that takes the wrapped type @m a@ to @n a@ in an Applicative
-- @f@ for all @a@, visit all contained @m@s to convert from @t m@ to @t n@.
--
-- @m@ and @n@ here play the role of @a@ and @b@ in the normal 'traverse' type;
-- that is, instead of traversing to change a @Type@, we're traversing to change
-- a wrapper type constructor of kind @k -> Type@:
--
--     traverse
--       :: (Traversable t, Applicative f)
--       => (          a   -> f  b   ) -> t a -> f (t b)
--     traverse10
--       :: (Traversable10 t, Applicative f)
--       => (forall a. m a -> f (n a)) -> t m -> f (t n)
--
-- An equivalent type signature is:
--
--     traverse10 :: Traversable10 t => Traversal10 (t m) (t n) m n
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

sequenceA10
  :: (Applicative f, Traversable10 t)
  => t (f :.: g) -> f (t g)
sequenceA10 = traverse10 coerce

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

-- TODO(awpr):
--
-- class Antimonoidal10 f where -- (Alternative10)
--   nah10 :: f (Const Void)
--   alt10 :: f m -> f m -> f (Sum m n)
--
-- class Contravariant10 f where
--   contramap10 :: (forall a. n a -> m a) -> f m -> f n

data Constrained (cxt :: k -> Constraint) (m :: k -> Type) (a :: k) where
  Constrained :: forall cxt m a. cxt a => m a -> Constrained cxt m a
  deriving Pretty via WrappedPortray (Constrained cxt m a)

instance Portray (m a) => Portray (Constrained cxt m a) where
  portray (Constrained x) = portray x


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
--
-- The minimal definition is actually 'constrained10' if @f@ is 'Functor10',
-- and 'unconstrained10' if @f@ is @Contravariant10@.  Neither is needed if @f@
-- is phantom (and thus both covariant and contravariant).
class Constrained10 (cxt :: k -> Constraint) (f :: (k -> Type) -> Type) where
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

instance (Constrained10 cxt f, Constrained10 cxt g) => Constrained10 cxt (f :+: g) where
  constrained10 (L1 x) = L1 (constrained10 x)
  constrained10 (R1 x) = R1 (constrained10 x)

  unconstrained10 (L1 x) = L1 (unconstrained10 x)
  unconstrained10 (R1 x) = R1 (unconstrained10 x)

instance (Constrained10 cxt f, Constrained10 cxt g) => Constrained10 cxt (f :*: g) where
  constrained10 (l :*: r) = constrained10 l :*: constrained10 r
  unconstrained10 (l :*: r) = unconstrained10 l :*: unconstrained10 r

instance (Functor f, Constrained10 cxt g) => Constrained10 cxt (f :.: g) where
  constrained10 (Comp1 f) = Comp1 (fmap constrained10 f)
  unconstrained10 (Comp1 f) = Comp1 (fmap unconstrained10 f)

-- | 'fmap10' with access to a constraint for every element.
fmap10C
  :: forall cxt f m n
   . (Constrained10 cxt f, Functor10 f)
  => (forall a. cxt a => m a -> n a) -> f m -> f n
fmap10C f = fmap10 (withConstrained @cxt f) . constrained10

-- | ('<$!') with access to a constraint for every element.
--
-- It might not be possible to use this as an operator because it needs
-- TypeApplications to determine @cxt@.
(<$?)
  :: forall cxt f m n
   . (Constrained10 cxt f, Functor10 f)
  => (forall a. n a) -> f m -> f n
(<$?) x = fmap10C @cxt (const x)

-- | ('<$>!') with access to a constraint for every element.
--
-- It might not be possible to use this as an operator because it needs
-- TypeApplications to determine @cxt@.
(<$>?)
  :: forall cxt f m n
   . (Constrained10 cxt f, Functor10 f)
  => (forall a. cxt a => m a -> n a) -> f m -> f n
(<$>?) = fmap10C @cxt

-- | 'foldMap10' with access to a constraint for every element.
foldMap10C
  :: forall cxt t m w
   . (Constrained10 cxt t, Foldable10 t, Monoid w)
  => (forall a. cxt a => m a -> w) -> t m -> w
foldMap10C f = foldMap10 (withConstrained @cxt f) . constrained10

-- | 'foldr10' with access to a constraint for every element.
foldr10C
  :: forall cxt t m b
   . (Constrained10 cxt t, Foldable10 t)
  => (forall a. cxt a => m a -> b -> b) -> b -> t m -> b
foldr10C f z = foldr10 (withConstrained @cxt f) z . constrained10

-- | 'foldl10' with access to a constraint for every element.
foldl10C
  :: forall cxt t m b
   . (Constrained10 cxt t, Foldable10 t)
  => (forall a. cxt a => b -> m a -> b) -> b -> t m -> b
foldl10C f z = foldl10 (flip (withConstrained @cxt (flip f))) z . constrained10

-- | 'traverse10_' with access to a constraint for every element.
traverse10C_
  :: forall cxt f t m n
   . (Constrained10 cxt t, Applicative f, Foldable10 t)
  => (forall a. cxt a => m a -> f (n a)) -> t m -> f ()
traverse10C_ f =
  --          ewwwwwwwwww
  traverse10_ @_ @_ @_ @n (withConstrained @cxt f) . constrained10

-- | 'traverse10' with access to a constraint for every element.
traverse10C
  :: forall cxt f t m n
   . (Constrained10 cxt t, Applicative f, Traversable10 t)
  => (forall a. cxt a => m a -> f (n a)) -> t m -> f (t n)
traverse10C f = traverse10 (withConstrained @cxt f) . constrained10

-- | 'pure10' with access to a constraint for every element.
pure10C
  :: forall cxt f m
   . (Constrained10 cxt f, Applicative10 f)
  => (forall a. cxt a => m a) -> f m
pure10C x = fmap10C @cxt (const x) (pure10 Proxy)

-- | 'liftA210' with access to a constraint for every element.
liftA210C
  :: forall cxt f m n o
   . (Constrained10 cxt f, Applicative10 f)
  => (forall a. cxt a => m a -> n a -> o a)
  -> f m -> f n -> f o
liftA210C f = liftA210 (withConstrained @cxt f) . constrained10

-- | 'liftA310' with access to a constraint for every element.
liftA310C
  :: forall cxt f m n o p
   . (Constrained10 cxt f, Applicative10 f)
  => (forall a. cxt a => m a -> n a -> o a -> p a)
  -> f m -> f n -> f o -> f p
liftA310C f = liftA310 (withConstrained @cxt f) . constrained10

-- | Analogous to 'Representable' for 'Functor10'.
--
-- If @f@ is @Representable10@, then a value of type @f m@ is isomorphic to a
-- function @forall a. Rep10 a -> m a@.  This essentially means it can be
-- thought of as a fixed-shape record with a wrapper type applied to all of its
-- fields.
--
-- This is also equivalent to a total dependent map (see the dependent-map
-- package) from @Rep10 f@ to @m@ ("total" meaning that every "key" has a
-- "value").
class Applicative10 f => Representable10 (f :: (k -> Type) -> Type) where
  type Rep10 f :: k -> Type
  index10 :: f m -> Rep10 f a -> m a
  tabulate10 :: (forall a. Rep10 f a -> m a) -> f m

  update10 :: Rep10 f a -> m a -> f m -> f m

rep10' :: Representable10 f => (f (Rep10 f) -> Rep10 f a) -> Rep10 f a
rep10' = ($ tabulate10 id)

rep10
  :: Representable10 f
  => Getting (Rep10 f a) (f (Rep10 f)) (Rep10 f a) -> Rep10 f a
rep10 l = rep10' (view l)

index10C
  :: forall cxt f a r m
   . (Representable10 f, Constrained10 cxt f)
  => f m -> Rep10 f a -> (cxt a => m a -> r) -> r
index10C fm k f = withConstrained @cxt f $ index10 (constrained10 @_ @cxt fm) k

ix10 :: Representable10 f => Rep10 f a -> Lens' (f m) (m a)
ix10 i = lens (`index10` i) (flip (update10 i))

-- | 'fmap10' with an index parameter.
--
-- The additional 'Rep10' parameter is a GADT-like type that identifies the
-- current field within @f@ and its type.  Frequently this will be
-- @'Field10' f@, but it may also be an actual hand-coded GADT.
imap10
  :: Representable10 f
  => (forall a. Rep10 f a -> m a -> n a) -> f m -> f n
imap10 f fm = tabulate10 (\i -> f i (fm `index10` i))

-- | 'foldMap10' with an index parameter.
--
-- See 'fmap10'.
ifoldMap10
  :: (Monoid w, Foldable10 f, Representable10 f)
  => (forall a. Rep10 f a -> m a -> w) -> f m -> w
ifoldMap10 f fm = fold10 $ imap10 (Constant .: f) fm

-- | 'foldl10' with an index parameter.
ifoldl10
  :: (Foldable10 f, Representable10 f)
  => (forall a. Rep10 f a -> b -> m a -> b) -> b -> f m -> b
ifoldl10 f z fm = appEndo (ifoldMap10 (\i x -> Endo (\b -> f i b x)) fm) z

-- | 'foldr10' with an index parameter.
ifoldr10
  :: (Foldable10 f, Representable10 f)
  => (forall a. Rep10 f a -> m a -> b -> b) -> b -> f m -> b
ifoldr10 f z fm = flip appEndo z $ getDual $
  ifoldMap10 (\i x -> Dual $ Endo (f i x)) fm

-- | 'traverse10' with an index parameter.
--
-- See 'fmap10'.
itraverse10
  :: (Applicative f, Traversable10 t, Representable10 t)
  => (forall a. Rep10 t a -> m a -> f (n a))
  -> t m -> f (t n)
itraverse10 f fm = sequenceA10 $ imap10 (Comp1 .: f) fm

-- | Analog of 'distributeRep' for 'Representable10'.
--
-- Pulls a fixed record shape to the outside of any functor.
distributeRep10
  :: (Representable10 f, Functor w)
  => w (f m) -> f (w :.: m)
distributeRep10 wfm = tabulate10 (\r -> Comp1 $ (`index10` r) <$> wfm)

-- | Analog of 'collectRep' for 'Representable10'.
--
-- Gathers a fixed record shape mapped over the elements of any functor.
collectRep10
  :: (Representable10 f, Functor w)
  => (a -> f m) -> w a -> f (w :.: m)
collectRep10 f wa = distributeRep10 (f <$> wa)

-- | A 'Functor10' made by applying the argument to a fixed type.
newtype Ap10 (a :: k) (f :: k -> Type) = Ap10 { unAp10 :: f a }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Pretty, Portray)

instance (NFData a, NFData (f a)) => NFData (Ap10 a f)

instance Functor10 (Ap10 a) where
  fmap10 f (Ap10 x) = Ap10 (f x)

instance Foldable10 (Ap10 a) where
  foldMap10 f (Ap10 x) = f x

instance Traversable10 (Ap10 a) where
  mapTraverse10 r f (Ap10 x) = r . Ap10 <$> f x

instance Applicative10 (Ap10 a) where
  pure10 = Ap10
  liftA210 f (Ap10 x) (Ap10 y) = Ap10 $ f x y

instance cxt a => Constrained10 cxt (Ap10 a) where
  constrained10 (Ap10 x) = Ap10 (Constrained x)

-- | A 'Functor10' made by applying the argument to the first of two types.
newtype Const10 (a :: k1) (b :: k2) (f :: k1 -> Type) = Const10 (f a)
  deriving newtype Portray
  deriving Pretty via WrappedPortray (Const10 a b f)

instance Functor10 (Const10 a b) where
  fmap10 f (Const10 x) = Const10 (f x)

instance Foldable10 (Const10 a b) where
  foldMap10 f (Const10 x) = f x

instance Traversable10 (Const10 a b) where
  mapTraverse10 r f (Const10 x) = r . Const10 <$> f x

instance cxt a => Constrained10 cxt (Const10 a b) where
  constrained10 (Const10 x) = Const10 (Constrained x)

-- | A 'Functor10' made by applying the argument to each of two fixed types.
data Pair10 (a :: k) (b :: k) (f :: k -> Type) = Pair10 (f a) (f b)
  deriving (Generic, Eq, Show)
  deriving Portray via Wrapped Generic (Pair10 a b f)
  deriving Pretty via WrappedPortray (Pair10 a b f)

instance Functor10 (Pair10 a b) where
  fmap10 f (Pair10 x y) = Pair10 (f x) (f y)

instance Foldable10 (Pair10 a b) where
  foldMap10 f (Pair10 x y) = f x <> f y

instance Traversable10 (Pair10 a b) where
  mapTraverse10 r f (Pair10 x y) = (\x' y' -> r (Pair10 x' y')) <$> f x <*> f y

instance Applicative10 (Pair10 a b) where
  pure10 x = Pair10 x x
  liftA210 f (Pair10 ma mb) (Pair10 na nb) = Pair10 (f ma na) (f mb nb)

instance (cxt a, cxt b) => Constrained10 cxt (Pair10 a b) where
  constrained10 (Pair10 x y) = Pair10 (Constrained x) (Constrained y)

-- | A 'Functor10' made by applying the argument to a fixed type, optionally.
data Maybe10 (a :: k) (f :: k -> Type) = Nothing10 | Just10 (f a)
  deriving Generic
  deriving Portray via Wrapped Generic (Maybe10 a f)
  deriving Pretty via WrappedPortray (Maybe10 a f)

instance Functor10 (Maybe10 a) where
  fmap10 _ Nothing10 = Nothing10
  fmap10 f (Just10 x) = Just10 (f x)

instance Foldable10 (Maybe10 a) where
  foldMap10 _ Nothing10 = mempty
  foldMap10 f (Just10 x) = f x

instance Traversable10 (Maybe10 a) where
  mapTraverse10 r _ Nothing10 = pure (r Nothing10)
  mapTraverse10 r f (Just10 x) = r . Just10 <$> f x

-- Note: Maybe10 isn't Applicative10 (the related Applicative10 thing is
-- MaybeF10).

instance cxt a => Constrained10 cxt (Maybe10 a) where
  constrained10 Nothing10 = Nothing10
  constrained10 (Just10 x) = Just10 (Constrained x)

-- | A 'Functor10' made by applying the argument to one of two fixed types.
data Either10 (a :: k) (b :: k) (f :: k -> Type) = Left10 (f a) | Right10 (f b)
  deriving Generic
  deriving Portray via Wrapped Generic (Either10 a b f)
  deriving Pretty via WrappedPortray (Either10 a b f)

instance Functor10 (Either10 a b) where
  fmap10 f (Left10 x) = Left10 (f x)
  fmap10 f (Right10 x) = Right10 (f x)

instance Foldable10 (Either10 a b) where
  foldMap10 f (Left10 x) = f x
  foldMap10 f (Right10 x) = f x

instance Traversable10 (Either10 a b) where
  mapTraverse10 r f (Left10 x)  = r . Left10 <$> f x
  mapTraverse10 r f (Right10 x) = r . Right10 <$> f x

-- Note: Either10 isn't Applicative10 (the related Applicative10 thing is
-- EitherF10).

instance (cxt a, cxt b) => Constrained10 cxt (Either10 a b) where
  constrained10 (Left10 x) = Left10 (Constrained x)
  constrained10 (Right10 x) = Right10 (Constrained x)

-- TODO(awpr): is this at all useful?  It's just the same as [f a], and fmap10
-- does the same as fmap would.
-- data List10 a f = Nil10 | Cons10 (f a) (List10 a f)
-- (wrengr): no, the generalization you want is:
-- > data List (f :: k -> Type) :: [k] -> Type where
-- >     Nil  :: List f '[]
-- >     Cons :: f x -> List f xs -> List f (x ': xs)
-- Of course, that requires making all the infrastructure here
-- polykinded. Also I think we have a version of this data type already
-- in the codebase somewhere.

-- | A 'Functor10' made by appling the second argument to a fixed type.
newtype ConstF10 (a :: k1) (f :: k1 -> Type) (g :: k2 -> Type) = ConstF10 (f a)
  deriving Generic
  deriving Portray via Wrapped Generic (ConstF10 a f g)
  deriving Pretty via WrappedPortray (ConstF10 a f g)

instance Functor10 (ConstF10 a f) where
  fmap10 _ (ConstF10 x) = ConstF10 x

instance Foldable10 (ConstF10 a f) where
  foldMap10 _ _ = mempty

instance Traversable10 (ConstF10 a f) where
  mapTraverse10 r _ (ConstF10 x) = pure (r $ ConstF10 x)

instance Constrained10 cxt (ConstF10 a f) where
  constrained10 (ConstF10 x) = ConstF10 x


-- | A 'Functor10' made by applying both arguments to a fixed type.
data PairF10 (a :: k) (f :: k -> Type) (g :: k -> Type) = PairF10 (f a) (g a)
  deriving Generic
  deriving Portray via Wrapped Generic (PairF10 a f g)
  deriving Pretty via WrappedPortray (PairF10 a f g)

instance Functor10 (PairF10 a f) where
  fmap10 f (PairF10 x y) = PairF10 x (f y)

instance Foldable10 (PairF10 a f) where
  foldMap10 f (PairF10 _ y) = f y

instance Traversable10 (PairF10 a f) where
  mapTraverse10 r f (PairF10 x y) = r . PairF10 x <$> f y

instance cxt a => Constrained10 cxt (PairF10 a f) where
  constrained10 (PairF10 x y) = PairF10 x (Constrained y)


-- | A 'Functor10' made by applying one of the two arguments to a fixed type.
data EitherF10 (a :: k) (f :: k -> Type) (g :: k -> Type)
  = LeftF10 (f a)
  | RightF10 (g a)
  deriving Generic
  deriving Portray via Wrapped Generic (EitherF10 a f g)
  deriving Pretty via WrappedPortray (EitherF10 a f g)

instance Functor10 (EitherF10 a f) where
  fmap10 _ (LeftF10 x) = LeftF10 x
  fmap10 f (RightF10 x) = RightF10 (f x)

instance Foldable10 (EitherF10 a f) where
  foldMap10 _ (LeftF10 _) = mempty
  foldMap10 f (RightF10 x) = f x

instance Traversable10 (EitherF10 a f) where
  mapTraverse10 r _ (LeftF10 x) = pure (r $ LeftF10 x)
  mapTraverse10 r f (RightF10 x) = r . RightF10 <$> f x

instance cxt a => Constrained10 cxt (EitherF10 a f) where
  constrained10 (LeftF10 x) = LeftF10 x
  constrained10 (RightF10 x) = RightF10 (Constrained x)

-- Poly-kinded 'Compose' is (:.:)
--
-- Several instantiations of this have the right kind for a Functor10 instance:
-- - Comp {k ~ Type -> Type} {j ~ Type -> Type}
-- - Comp {k ~ Type} {j ~ Type -> Type}
--
-- The latter instance is actually implemented, but the former requires a
-- functor-like class for @(Type -> Type) -> (Type -> Type)@, which we don't
-- currently have.

comp :: Iso ((f :.: g) a) ((h :.: i) b) (f (g a)) (h (i b))
comp = iso unComp1 Comp1

-- | 'Product' on 'Functor10's.
data ProductF10 f g (m :: k -> Type) = ProductF10 (f m) (g m)
  deriving Generic
  deriving Portray via Wrapped Generic (ProductF10 f g m)
  deriving Pretty via WrappedPortray (ProductF10 f g m)

instance (Functor10 f, Functor10 g) => Functor10 (ProductF10 f g) where
  fmap10 f (ProductF10 x y) = ProductF10 (fmap10 f x) (fmap10 f y)

instance (Foldable10 f, Foldable10 g) => Foldable10 (ProductF10 f g) where
  foldMap10 f (ProductF10 x y) = foldMap10 f x <> foldMap10 f y

instance (Traversable10 f, Traversable10 g) => Traversable10 (ProductF10 f g) where
  mapTraverse10 r f (ProductF10 x y) =
    mapTraverse10 (r .: ProductF10) f x <*> traverse10 f y

instance (Applicative10 f, Applicative10 g)
    => Applicative10 (ProductF10 f g) where
  pure10 x = ProductF10 (pure10 x) (pure10 x)
  liftA210 f (ProductF10 xl xr) (ProductF10 yl yr) =
    ProductF10 (liftA210 f xl yl) (liftA210 f xr yr)

instance (Constrained10 cxt f, Constrained10 cxt g)
    => Constrained10 cxt (ProductF10 f g) where
  constrained10 (ProductF10 x y) =
    ProductF10 (constrained10 x) (constrained10 y)
  unconstrained10 (ProductF10 x y) =
    ProductF10 (unconstrained10 x) (unconstrained10 y)


-- | 'Sum' on 'Functor1's.
data SumF10 f g (m :: k -> Type) = InLF10 (f m) | InRF10 (g m)
  deriving Generic
  deriving Portray via Wrapped Generic (SumF10 f g m)
  deriving Pretty via WrappedPortray (SumF10 f g m)

instance (Functor10 f, Functor10 g) => Functor10 (SumF10 f g) where
  fmap10 f (InLF10 x) = InLF10 (fmap10 f x)
  fmap10 f (InRF10 x) = InRF10 (fmap10 f x)

instance (Foldable10 f, Foldable10 g) => Foldable10 (SumF10 f g) where
  foldMap10 f (InLF10 x) = foldMap10 f x
  foldMap10 f (InRF10 x) = foldMap10 f x

instance (Traversable10 f, Traversable10 g) => Traversable10 (SumF10 f g) where
  mapTraverse10 r f (InLF10 x) = mapTraverse10 (r . InLF10) f x
  mapTraverse10 r f (InRF10 x) = mapTraverse10 (r . InRF10) f x

instance (Constrained10 cxt f, Constrained10 cxt g)
    => Constrained10 cxt (SumF10 f g) where
  constrained10 (InLF10 x) = InLF10 (constrained10 x)
  constrained10 (InRF10 x) = InRF10 (constrained10 x)

  unconstrained10 (InLF10 x) = InLF10 (unconstrained10 x)
  unconstrained10 (InRF10 x) = InRF10 (unconstrained10 x)


-- | A 'Functor1' made by applying the argument to an existential type.
data Exists (m :: k -> Type) where
  Exists :: forall a m. m a -> Exists m

deriving stock instance (forall a. Show (m a)) => Show (Exists m)

instance (forall a. Portray (m a)) => Portray (Exists m) where
  portray (Exists x) = Apply (Atom "Exists") [portray x]

deriving via WrappedPortray (Exists m)
  instance (forall a. Portray (m a)) => Pretty (Exists m)

instance Functor10 Exists where
  fmap10 f (Exists x) = Exists (f x)

instance Foldable10 Exists where
  foldMap10 f (Exists x) = f x

instance Traversable10 Exists where
  mapTraverse10 r f (Exists x) = r . Exists <$> f x

ap10 :: Iso (Ap10 s fs) (Ap10 t ft) (fs s) (ft t)
ap10 = iso unAp10 Ap10

deriving instance Default (f a) => Default (Ap10 a f)
