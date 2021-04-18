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

-- | Extends 'Representable' with support for modifying elements.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Update
         ( Update(..), updateRep, ixRep, GUpdate(..)
         , equalityTable
         ) where

import Data.Coerce (coerce)
import Data.Functor ((<&>))
import GHC.Generics
         ( Generic1(..)
         , (:*:)(..), (:.:)(..)
         , M1(..), Rec1(..), U1(..), Par1(..)
         )

import Data.Functor.Rep (Representable(..))

import Data.Functor.Field (Field(..), GTabulate(..), FieldRep(..))
import Data.Ten.Internal (mapStarFst, mapStarSnd)

class Representable f => Update f where
  overRep :: Rep f -> (a -> a) -> f a -> f a

-- | Update an @f a@ at a given index.
updateRep :: Update f => Rep f -> a -> f a -> f a
updateRep i = overRep i . const

-- | A 'Control.Lens.Lens' to the field identified by a given 'Rep'.
--
-- @
--     ixRep :: Update f => Rep f -> Lens' (f a) a
-- @
ixRep :: (Update f, Functor m) => Rep f -> (a -> m a) -> f a -> m (f a)
ixRep i f = \fa -> f (index fa i) <&> \ma -> updateRep i ma fa

instance (Generic1 f, GTabulate (Rep1 f), GUpdate (Rep1 f), Functor f)
      => Update (FieldRep f) where
  overRep =
    \i f (FieldRep fa) -> FieldRep $ runFS (getField i setters_) f fa
   where
    setters_ :: f (FieldSetter f)
    setters_ = setters

equalityTable :: Update f => f (f Bool)
equalityTable = tabulate (\i -> updateRep i True (tabulate (const False)))

-- | The 'Generic1' implementation of 'GUpdate'.
--
-- As with 'GTabulate00', this is used rather than 'GUpdate' to handle
-- sub-records, so derive this in order to allow using your type as a nested
-- record.
class GUpdate rec where
  gsetters :: ((forall a. (a -> a) -> rec a -> rec a) -> r) -> rec r

instance GUpdate U1 where
  gsetters _r = U1
  {-# INLINE gsetters #-}

instance GUpdate rec => GUpdate (Rec1 rec) where
  gsetters r = Rec1 $ gsetters (\s -> r $ \f -> Rec1 . s f . unRec1)
  {-# INLINE gsetters #-}

instance GUpdate rec => GUpdate (M1 k i rec) where
  gsetters r = M1 $ gsetters (\s -> r $ \f -> M1 . s f . unM1)
  {-# INLINE gsetters #-}

instance GUpdate Par1 where
  gsetters r = Par1 $ r $ \f -> Par1 . f . unPar1
  {-# INLINE gsetters #-}

instance (GUpdate f, GUpdate g) => GUpdate (f :*: g) where
  gsetters r = fs :*: gs
   where
    fs = gsetters $ \s -> r $ mapStarFst . s
    gs = gsetters $ \s -> r $ mapStarSnd . s
  {-# INLINE gsetters #-}

instance (GUpdate f, GUpdate g) => GUpdate (f :.: g) where
  gsetters r = Comp1 $
    gsetters $ \ s0 ->
    gsetters $ \ s1 ->
    r $ \f -> coerce (s0 (s1 f))
  {-# INLINE gsetters #-}

newtype FieldSetter f = FS { runFS :: forall a. (a -> a) -> f a -> f a }

setters :: (Generic1 f, GUpdate (Rep1 f)) => f (FieldSetter f)
setters = to1 $ gsetters (\overI -> FS $ \f -> to1 . overI f . from1)
