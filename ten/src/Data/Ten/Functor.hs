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

-- | A functor from the category @* -> Type@ to the category @*@.
class Functor10 (f :: (k -> Type) -> Type) where
  fmap10 :: (forall a. m a -> n a) -> f m -> f n
  -- default fmap10 :: Traversable10 f => (forall a. m a -> n a) -> f m -> f n
  -- fmap10 f = runIdentity . traverse10 (Identity . f)

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

-- | 'void' for 'Functor10'.
--
-- This returns @f 'Proxy'@ because @Proxy :: k -> Type@ has the right kind and
-- carries no runtime information.  It's isomorphic to @Const ()@ but easier to
-- spell.
void10 :: Functor10 f => f m -> f Proxy
void10 = fmap10 (const Proxy)
