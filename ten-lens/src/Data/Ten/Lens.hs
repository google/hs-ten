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

-- | Provides lenses and related functionality for the "ten" package.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Ten.Lens
         ( rep10, field10, ixRep10, ap10, comp
         , _Field10, _Field10', (!=), (!=?), fragmented
         ) where

import Data.Functor.Contravariant (contramap)
import Data.Type.Equality (TestEquality(..), (:~:)(..))

import Control.Lens (Getting, Prism', Iso, iso, view)
import Control.Lens.Setter ((%~), Setter, ASetter, setting)
import Data.Profunctor (dimap, right')

import Data.Ten.Ap (Ap10(..))
import Data.Ten.Sigma ((:**)(..), OpCostar, lmapFragment)
import Data.Ten.Representable (Representable10(..), rep10', field10')
import Data.Ten.Update (ixRep10)
import Data.Ten ((:.:)(..))

-- | Turn a record field lens into a 'Rep10'.
--
-- Since 'tabulate10' can give us a record of 'Rep10's, all we have to do to
-- convert a lens into a 'Rep10' is use 'view' to extract the desired 'Rep10'.
rep10
  :: Representable10 f
  => Getting (Rep10 f a) (f (Rep10 f)) (Rep10 f a) -> Rep10 f a
rep10 l = rep10' (view l)

-- | Convert a lens targeting Ap10 to a Rep10.
field10
  :: Representable10 rec
  => Getting (Ap10 a (Rep10 rec)) (rec (Rep10 rec)) (Ap10 a (Rep10 rec))
  -> Rep10 rec a
field10 l = field10' (view l)

-- | An 'Iso' between an @Ap10 a m@ wrapper and its contained @m a@.
ap10 :: Iso (Ap10 s fs) (Ap10 t ft) (fs s) (ft t)
ap10 = iso unAp10 Ap10

-- | An 'Iso' between a @(m :.: n) a@ wrapper and its contained @m (n a)@.
comp :: Iso ((m :.: n) a) ((k :.: l) b) (m (n a)) (k (l b))
comp = iso unComp1 Comp1

-- | A 'Prism' from a ':**" to a particular field.
--
--     _Field10 k f (k := m) === (k :=) <$> f m
--     _Field10 k' f (k := m) === k := m | k' /= k
--     _Field10 k # m === k := m
_Field10
  :: TestEquality k
  => k a -> Prism' (k :** m) (m a)
_Field10 k = dimap toE fromE . right'
 where
  toE frag@(k' :** m) = case testEquality k k' of
    Just Refl -> Right m
    Nothing -> Left frag

  fromE = either pure (fmap (k :**))

-- | '_Field10' taking the field lens rather than the 'Rep10'.
_Field10'
  :: forall rec a m
   . (TestEquality (Rep10 rec), Representable10 rec)
  => (forall n. Getting (Ap10 a n) (rec n) (Ap10 a n))
  -> Prism' (Rep10 rec :** m) (m a)
_Field10' l = _Field10 @(Rep10 rec) (field10 l)

infixr 5 !=
-- | Shortcut to construct a (':**') from a 'Getter'.
--
-- Note that this assumes the fields are ultimately wrapped in 'Ap10'.  If a
-- particular field doesn't have 'Ap10' (which can only arise from a
-- manually-written 'Record10' instance), just pretend it does by adding
-- @from ap10@ to the lens.
(!=)
  :: Representable10 rec
  => (forall m. Getting (Ap10 a m) (rec m) (Ap10 a m))
  -> f a -> Rep10 rec :** f
l != x = field10 l :** x

infixr 5 !=?
-- | Shortcut to construct a (':**') using 'pure' for the value.
(!=?)
  :: (Representable10 rec, Applicative f)
  => (forall m. Getting (Ap10 a m) (rec m) (Ap10 a m))
  -> a -> Rep10 rec :** f
l !=? x = l != pure x

-- | Lifts a 'Setter' to work underneath (':**').
--
-- This means if you know how to change the type of a whole record, you can use
-- this to change the type of a (':**').
--
-- Example usage:
--
--     data MyRecord a m = MyRecord { _mrA :: Ap10 a m, _mrInt :: Ap10 Int m }
--     mrA :: Lens' (MyRecord a m) (MyRecord b m) (Ap10 m a) (Ap10 m b)
--
--     example
--       :: Rep10 (MyRecord Int) :** Identity
--       -> Rep10 (MyRecord String) :** Identity
--     example = fragmented (mrA.ap10) %~ show
fragmented
  :: ( Functor m, Representable10 recA, Representable10 recB
     , f ~ OpCostar m (Rep10 recB :** m)
     )
  => ASetter (recB f) (recA f) (f b) (f a)
  -> Setter (Rep10 recA :** m) (Rep10 recB :** m) a b
fragmented l = setting $ \f -> lmapFragment (l %~ contramap f)
