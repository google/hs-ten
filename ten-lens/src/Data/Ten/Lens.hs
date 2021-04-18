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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Ten.Lens (rep10, ix10, ap10, comp) where

import Control.Lens (Lens', Getting, Iso, iso, lens, view)

import Data.Ten.Representable (Representable10(..), rep10')
import Data.Ten (Ap10(..), (:.:)(..))

-- | Turn a record field lens into a 'Rep10'.
--
-- Since 'tabulate10' can give us a record of 'Rep10's, all we have to do to
-- convert a lens into a 'Rep10' is use 'view' to extract the desired 'Rep10'.
rep10
  :: Representable10 f
  => Getting (Rep10 f a) (f (Rep10 f)) (Rep10 f a) -> Rep10 f a
rep10 l = rep10' (view l)

-- | A 'Control.Lens.Lens' to the field identified by a given 'Rep10'.
ix10 :: Representable10 f => Rep10 f a -> Lens' (f m) (m a)
ix10 i = lens (`index10` i) (flip (update10 i))

-- | An 'Iso' between an @Ap10 a m@ wrapper and its contained @m a@.
ap10 :: Iso (Ap10 s fs) (Ap10 t ft) (fs s) (ft t)
ap10 = iso unAp10 Ap10

-- | An 'Iso' between a @(m :.: n) a@ wrapper and its contained @m (n a)@.
comp :: Iso ((m :.: n) a) ((k :.: l) b) (m (n a)) (k (l b))
comp = iso unComp1 Comp1
