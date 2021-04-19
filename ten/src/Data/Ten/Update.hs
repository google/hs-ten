-- Copyright 2021 Google LLC
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

-- | Extends 'Representable10' with support for modifying elements.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Update
         ( Update10(..), updateRep10, ixRep10, FieldSetter10(..)
         , EqualityTable(..), equalityTable
         , GUpdate10(..)
         ) where

import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..))
import GHC.Generics
         ( Generic1(..)
         , (:*:)(..), (:.:)(..)
         , M1(..), Rec1(..), U1(..)
         )

import Data.Wrapped (Wrapped1(..))

import Data.Functor.Rep (Representable(..))

import Data.Functor.Update (Update(..))
import Data.Ten.Ap (Ap10(..))
import Data.Ten.Applicative (Applicative10(..))
import Data.Ten.Field (Field10(..))
import Data.Ten.Internal (mapStarFst, mapStarSnd)
import Data.Ten.Representable (Representable10(..), GTabulate10(..))

-- | Extends 'Representable10' with support for modifying elements.
--
-- See also 'Data.Functor.Update.Update'.
class Representable10 f => Update10 (f :: (k -> Type) -> Type) where
  -- | Modify an @f m@ at a given index.
  overRep10 :: Rep10 f a -> (m a -> m a) -> f m -> f m

-- | Update an @f m@ at a given index.
updateRep10 :: Update10 f => Rep10 f a -> m a -> f m -> f m
updateRep10 i = overRep10 i . const

-- | A 'Control.Lens.Lens' to the field identified by a given 'Rep10'.
--
-- @
--     ix10 :: Update10 f => Rep10 f a -> Lens' (f m) (m a)
-- @
ixRep10
  :: (Update10 f, Functor g)
  => Rep10 f a -> (m a -> g (m a)) -> f m -> g (f m)
ixRep10 i f = \fm -> f (index10 fm i) <&> \fma -> updateRep10 i fma fm

-- | A newtype wrapper to store tables of equality witnesses in @f@.
newtype EqualityTable f a = EqualityTable (f (Maybe :.: ((:~:) a)))

-- | Implementation detail of @'Data.Type.Equality.TestEquality' ('Field10' f)@.
--
-- This is a pre-populated table of @'Maybe' (a ':~:' b)@s, with 'Just's in the
-- elements where the inner position is the same as the outer position, i.e.
-- along the "diagonal".  Then we can test two @forall m. f m -> m a@ functions
-- for equality, by applying them in turn to the two layers of @f@, and see if
-- we reach a 'Just' or a 'Nothing'.
equalityTable :: Update10 f => f (EqualityTable f)
equalityTable = tabulate10 $ \i -> EqualityTable $
  updateRep10 i (Comp1 (Just Refl)) (pure10 (Comp1 Nothing))

instance ( Generic1 f
         , Applicative10 (Rep1 f), GTabulate10 (Rep1 f), GUpdate10 (Rep1 f)
         )
      => Update10 (Wrapped1 Generic1 f) where
  overRep10 =
    \i f (Wrapped1 fm) -> Wrapped1 $ runFS10 (getField10 i setters) f fm
   where
     -- Superstition-based optimization: try to make GHC specialize 'setters10'
     -- to @f@ exactly once per instance of 'Update10'.
     setters :: f (FieldSetter10 f)
     setters = setters10

-- | 'Generic1' implementation of 'Update10'.
class GUpdate10 (rec :: (k -> Type) -> Type) where
  gsetters10
    :: (forall a. (forall m. (m a -> m a) -> rec m -> rec m) -> r a)
    -> rec r

instance Update10 (Ap10 a) where
  overRep10 Refl f (Ap10 x) = Ap10 (f x)
  {-# INLINE overRep10 #-}

instance GUpdate10 U1 where
  gsetters10 _ = U1
  {-# INLINE gsetters10 #-}

instance Update10 rec => GUpdate10 (Rec1 rec) where
  gsetters10 r = Rec1 $ tabulate10 $
    \i -> r (\f -> Rec1 . overRep10 i f . unRec1)
  {-# INLINE gsetters10 #-}

instance GUpdate10 rec => GUpdate10 (M1 k i rec) where
  gsetters10 r = M1 $ gsetters10 (\s -> r $ \f -> M1 . s f . unM1 )
  {-# INLINE gsetters10 #-}

instance (GUpdate10 f, GUpdate10 g) => GUpdate10 (f :*: g) where
  gsetters10 r = fs :*: gs
   where
    fs = gsetters10 $ \s -> r $ mapStarFst . s
    gs = gsetters10 $ \s -> r $ mapStarSnd . s
  {-# INLINE gsetters10 #-}

instance (Update f, GUpdate10 g) => GUpdate10 (f :.: g) where
  gsetters10 r = Comp1 $
    tabulate $ \ i ->
    gsetters10 $ \ s ->
    r $ \f -> Comp1 . overRep i (s f) . unComp1
  {-# INLINE gsetters10 #-}

-- | A newtype wrapper to store field modifier functions in @f@.
newtype FieldSetter10 f a = FS10
  { runFS10 :: forall m. (m a -> m a) -> f m -> f m }

setters10 :: (Generic1 f, GUpdate10 (Rep1 f)) => f (FieldSetter10 f)
setters10 = to1 $ gsetters10 (\overI -> FS10 $ \f -> to1 . overI f . from1)
