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
    module Data.Ten.Functor
    -- ** Foldable10
  , module Data.Ten.Foldable
    -- ** Traversable10
  , module Data.Ten.Traversable
    -- ** Applicative10
  , module Data.Ten.Applicative
    -- ** Constrained10
  , module Data.Ten.Constrained
    -- ** Representable10
  , Representable10(..), index10C, ix10
  , imap10, ifoldMap10, ifoldl10, ifoldr10, itraverse10
  , rep10, rep10'
  , distributeRep10, collectRep10

    -- * Standard 'Functor10's
    -- ** Over types
  , Ap10(..), ap10, Const10(..), Pair10(..), Maybe10(..), Either10(..)
    -- ** Over type constructors
  , ConstF10(..), PairF10(..), EitherF10(..), Exists(..)
    -- ** Over 'Functor10's
  , (:.:)(..), comp
  , ProductF10(..), SumF10(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Functor.Constant (Constant(..))
import Data.Kind (Type)
import Data.Monoid (Dual(..), Endo(..))
import GHC.Generics

import Control.Lens (Lens', Getting, Iso, iso, lens, view)
import Data.Default.Class (Default)
import Data.Portray (Portray(..), Portrayal(..))
import Data.Portray.Pretty (WrappedPortray(..))
import Data.Wrapped (Wrapped(..))
import Text.PrettyPrint.HughesPJClass (Pretty)

import Data.Ten.Applicative
import Data.Ten.Constrained
import Data.Ten.Foldable
import Data.Ten.Functor
import Data.Ten.Traversable

(.:) :: (q -> r) -> (a -> b -> q) -> a -> b -> r
(.:) = (.) . (.)

-- TODO(awpr):
--
-- class Antimonoidal10 f where -- (Alternative10)
--   nah10 :: f (Const Void)
--   alt10 :: f m -> f m -> f (Sum m n)
--
-- class Contravariant10 f where
--   contramap10 :: (forall a. n a -> m a) -> f m -> f n

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
