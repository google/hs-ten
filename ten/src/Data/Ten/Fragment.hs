-- Copyright 2019-2021 Google LLC
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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A single field-value pair out of a 'Representable10' record.

module Data.Ten.Fragment
         ( Fragment(..), overFragment, lmapFragment, eqKey
         , OpFragment, OpCostar(..), caseFragment
         ) where

import Data.Functor.Contravariant (Contravariant(..))
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))

import Control.DeepSeq (NFData(..))
import Data.Portray (Portray(..), Portrayal(..), Infixity(..), Assoc(..))

import Data.Ten.Constrained (Constrained(..), Constrained10(..), pure10C)
import Data.Ten.Representable (Representable10(..))
import Data.Ten.Update (Update10, overRep10)

infixr 5 :=
-- | A pair of @k a@ and @m a@ for any (existential) @a@.
--
-- This is used to represent one field of a generic record, where @k@ is set to
-- @Rep10 f@.  The @k a@ identifies which field (and locks down its type), and
-- the @m a@ provides its value.
data Fragment rec m = forall a. Rep10 rec a := m a

withRepC
  :: forall c rec a r
   . (Representable10 rec, Constrained10 c rec)
  => Rep10 rec a -> (c a => r) -> r
withRepC k r = case d of Constrained _ -> r
 where
  d = pure10C @c @rec @(Constrained c Proxy) (Constrained Proxy) `index10` k

instance ( k ~ Rep10 rec, forall a. NFData (k a)
         , forall a. NFData a => NFData (m a)
         , Representable10 rec, Constrained10 NFData rec
         )
      => NFData (Fragment rec m) where
  rnf (k := m) = withRepC @NFData @rec k $ rnf k `seq` rnf m

instance ( k ~ Rep10 rec, TestEquality k
         , Representable10 rec, Constrained10 Eq rec
         , forall a. Eq a => Eq (m a)
         )
      => Eq (Fragment rec m) where
  (kl := ml) == (kr := mr) = case testEquality kl kr of
     Nothing -> False
     Just Refl -> withRepC @Eq @rec kl $ ml == mr

portrayFrag :: Portrayal -> Portrayal -> Portrayal
portrayFrag = Binop ":=" (Infixity AssocR 5)

{-
instance ( k ~ Rep10 rec
         , Arbitrary (Exists k)
         , Representable10 rec, Constrained10 Arbitrary rec
         , forall a. Arbitrary a => Arbitrary (m a)
         )
      => Arbitrary (Fragment rec m) where
  arbitrary = do
    Exists k <- arbitrary
    a <- withRepC @Arbitrary @rec k arbitrary
    return $ k := a
-}

instance ( k ~ Rep10 rec
         , forall a. Show (k a)
         , forall a. Show a => Show (m a)
         , Representable10 rec, Constrained10 Show rec
         )
      => Show (Fragment rec m) where
  -- We have to write this by hand because the derived version doesn't know how
  -- to call into 'Constrained10' to find a 'Show' instance for @a@ based on
  -- @ka@.
  showsPrec p (ka := ma) = showParen (p > prec) $
    showsPrec (1+prec) ka .
    showString " := " .
    withRepC @Show @rec ka (showsPrec (1+prec) ma)
   where
    prec = 5

instance ( k ~ Rep10 rec
         , forall a. Portray (k a)
         , forall a. Portray a => Portray (m a)
         , Representable10 rec, Constrained10 Portray rec
         )
      => Portray (Fragment rec m) where
  portray (ka := ma) = withRepC @Portray @rec ka $
    portrayFrag (portray ka) (portray ma)

-- | Check if two 'Fragment's have the same key.
eqKey :: TestEquality (Rep10 rec) => Fragment rec m -> Fragment rec n -> Bool
eqKey (kl := _) (kr := _) = case testEquality kl kr of
                              Nothing   -> False
                              Just Refl -> True

-- | "Zip" a single field of a record with a 'Fragment'.
--
-- Since we're only operating on a single field, the @n@ type can't vary like
-- in a traditional zip function.
overFragment
  :: Update10 rec
  => (forall a. m a -> n a -> n a) -> Fragment rec m -> rec n -> rec n
overFragment f (k := x) = overRep10 k (f x)

-- | Newtype used in implementing contravariant conversion of Fragments.  See
-- 'lmapFragment'.  Only exported because it's used in the type of
-- 'lmapFragment', but it can be largely ignored, like the many "ALens" etc.
-- types in "lens".
newtype OpFragment rec m a
  = OpFragment { runOpFragment :: m a -> Fragment rec m }

instance Functor m => Contravariant (OpFragment rec m) where
  contramap f = OpFragment . (. fmap f) . runOpFragment

-- | Convert a 'Fragment' to a different record type contravariantly.
--
-- Example usage:
--
--     data MyRecord1 m = MyRecord1 { _mr1A :: Ap10 Int m, _mr1B :: Ap10 Int m }
--     data MyRecord2 m = MyRecord2 { _mr2A :: Ap10 Int m }
--
--     -- Collapse both fields _mr1A and _mr1B onto _mr2A.
--     example
--       :: Fragment (Rep10 MyRecord1) Identity
--       -> Fragment (Rep10 MyRecord2) Identity
--     example = lmapFragment $ \MyRecord2{..} -> MyRecord1
--       { _mr1A = _mr2A
--       , _mr1B = _mr2A
--       }
--
-- It looks weird that the argument converts from 'recB' to 'recA' in order
-- to convert 'Fragment' the other way, so it merits some explanation: first,
-- note that, by @'Representable10' recA@, we know that @recA m@ is
-- isomorphic to @forall a. 'Rep10' recA a -> m a@.  That is, @Rep10 recA@
-- effectively appears in negative position in @recA m@.  So, a function from
-- @recB@ to @recA@ hand-wavingly contains a function in the opposite
-- direction from @Rep10 recA@ to @Rep10 recB@.
--
-- With the intuition out of the way, here's how we actually accomplish the
-- conversion: start off with a record @recB@ where each field is a function
-- that trivially rebuilds the corresponding @Fragment@ -- in each field with
-- @k :: Rep10 recB@ we literally just put @(k :=)@ with the appropriate
-- newtype constructors.  Then, apply the user's contravariant conversion
-- function, to turn our @recB@ of @recB@-fragment-builders into an
-- @recA@ of @recB@-fragment-builders.  If the user-provided conversion
-- function involves changing any field types, it must have done so by
-- @contramap@ping the fragment-builders: instead of a function that just
-- directly applies @(k :=)@ to its argument, they will now contain functions
-- equivalent to @\ma -> k := _f ma@.  Finally, unpack the @recA@ fragment
-- and use its @k@ to fetch that field's @recB@-fragment-builder (potentially
-- with a conversion inserted at the front), and apply it to the payload.
--
-- Usage will typically involve applying contramap to some number of fields and
-- leaving the rest unchanged.  If you have a type-changing 'Setter' at hand,
-- it's probably easier to use 'fragmented'.
lmapFragment
  :: forall recA recB m
   . (Representable10 recA, Representable10 recB)
  => (recB (OpFragment recB m) -> recA (OpFragment recB m))
  -> Fragment recA m -> Fragment recB m
lmapFragment f = \ (k := mx) -> runOpFragment (fragmentBuilders `index10` k) mx
 where
  fragmentBuilders :: recA (OpFragment recB m)
  fragmentBuilders = f (tabulate10 (\k' -> OpFragment (k' :=)))

newtype OpCostar f r a = OpCostar { getOpCostar :: f a -> r }

-- | Simulate a case statement on a 'Fragment' with a record of functions.
--
-- @
--     caseFragment (MyRecord1 (OpCostar isJust) (OpCostar isNothing)) x
-- @
--
-- Is analogous to (pseudo-code):
--
-- @
--     case x of { (_mr1A := mx) -> isJust mx; (_mr1B := mx) -> isNothing mx }
-- @
caseFragment
  :: Representable10 rec
  => rec (OpCostar f r)
  -> Fragment rec f
  -> r
caseFragment rec (k := v) = getOpCostar (rec `index10` k) v
