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
         , fold10, foldr10, foldl10, traverse10_, sequenceA10_
         ) where

import Data.Functor.Constant (Constant(..))
import Data.Kind (Type)
import Data.Monoid (Dual(..), Endo(..))
import GHC.Generics
         ( Generic1(..)
         , (:.:)(..), (:*:)(..), (:+:)(..)
         , M1(..), Rec1(..), U1(..), V1, K1(..)
         )

import Data.Wrapped (Wrapped1(..))


class Foldable10 (t :: (k -> Type) -> Type) where
  foldMap10 :: Monoid w => (forall a. m a -> w) -> t m -> w

instance (Generic1 f, Foldable10 (Rep1 f))
      => Foldable10 (Wrapped1 Generic1 f) where
  foldMap10 f = foldMap10 f . from1 . unWrapped1

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

