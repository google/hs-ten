{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Ten.Representable
         ( Representable10(..), index10C, ix10
         , imap10, ifoldMap10, ifoldl10, ifoldr10, itraverse10
         , rep10, rep10'
         , distributeRep10, collectRep10
         ) where

import Data.Functor.Constant (Constant(..))
import Data.Kind (Type)
import Data.Monoid (Dual(..), Endo(..))
import GHC.Generics ((:.:)(..))

import Control.Lens (Lens', Getting, lens, view)

import Data.Ten.Applicative (Applicative10)
import Data.Ten.Constrained (Constrained10(..), withConstrained)
import Data.Ten.Foldable (Foldable10, fold10)
import Data.Ten.Traversable (Traversable10, sequenceA10)

(.:) :: (q -> r) -> (a -> b -> q) -> a -> b -> r
(.:) = (.) . (.)

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

