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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An approximation of a dependent pair type.

module Data.Ten.Sigma
         ( (:**)(..), overFragment, lmapFragment, eqKey
         , OpCostar(..), caseFragment
         ) where

import Data.Functor.Contravariant (Contravariant(..))
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))

import Control.DeepSeq (NFData(..))
import Data.Portray (Portray(..), Portrayal(..), infixr_)

import Data.Ten.Entails ((:!:), Entails, withEntailment)
import Data.Ten.Foldable (Foldable10(..))
import Data.Ten.Foldable.WithIndex (Foldable10WithIndex(..))
import Data.Ten.Functor (Functor10(..))
import Data.Ten.Functor.WithIndex (Index10, Functor10WithIndex(..))
import Data.Ten.Representable (Representable10(..))
import Data.Ten.Traversable (Traversable10(..))
import Data.Ten.Traversable.WithIndex (Traversable10WithIndex(..))
import Data.Ten.Update (Update10, overRep10)

infixr 5 :**
-- | A pair of @k a@ and @m a@ for any (existential) @a@.
--
-- This is a lot like a dependent pair, in that it contains a left-hand-side
-- value that's meant to identify a type, and a right-hand-side parameterized
-- by that type.  For example, the true dependent pair type (in e.g. Idris)
-- @(n :: Nat ** Vec n Bool)@ could be approximated in Haskell as
-- @SInt :** Ap10 Bool Vec@.
--
-- This can be used to represent one field of a 'Representable10', where @k@ is
-- set to @Rep10 f@.  The @k a@ identifies which field (and locks down its
-- type), and the @m a@ provides its value.
data k :** m = forall a. k a :** m a

instance (forall a. NFData (k a), Entails k (NFData :!: m))
      => NFData (k :** m) where
  rnf (k :** m) = withEntailment @(NFData :!: m) k $ rnf k `seq` rnf m

instance (TestEquality k, Entails k (Eq :!: m))
      => Eq (k :** m) where
  (kl :** ml) == (kr :** mr) = case testEquality kl kr of
     Nothing -> False
     Just Refl -> withEntailment @(Eq :!: m) kl $ ml == mr

{-
instance ( Arbitrary (Exists k)
         , Representable10 rec, Entails k (Arbitrary :!: m)
         )
      => Arbitrary (k :** m) where
  arbitrary = do
    Exists k <- arbitrary
    a <- withEntailment @(Arbitrary :!: m) k arbitrary
    return $ k :** a
-}

instance (forall a. Show (k a), Entails k (Show :!: m))
      => Show (k :** m) where
  -- We have to write this by hand because the derived version doesn't know how
  -- to call into 'Constrained10' to find a 'Show' instance for @a@ based on
  -- @ka@.
  showsPrec p (ka :** ma) = showParen (p > prec) $
    showsPrec (1+prec) ka .
    showString " :** " .
    withEntailment @(Show :!: m) ka (showsPrec (1+prec) ma)
   where
    prec = 5

instance (forall a. Portray (k a), Entails k (Portray :!: m))
      => Portray (k :** m) where
  portray (ka :** ma) = withEntailment @(Portray :!: m) ka $
    Binop ":**" (infixr_ 5) (portray ka) (portray ma)

instance Functor10 ((:**) k) where fmap10 f (k :** m) = k :** f m
instance Foldable10 ((:**) k) where foldMap10 f (_ :** m) = f m
instance Traversable10 ((:**) k) where
  mapTraverse10 r f (k :** m) = r . (k :**) <$> f m

type instance Index10 ((:**) k) = k
instance Functor10WithIndex ((:**) k) where imap10 f (k :** m) = k :** f k m
instance Foldable10WithIndex ((:**) k) where ifoldMap10 f (k :** m) = f k m
instance Traversable10WithIndex ((:**) k) where
  imapTraverse10 r f (k :** m) = r . (k :**) <$> f k m

-- | Check if two pairs have the same key.
eqKey :: TestEquality k => k :** m -> k :** n -> Bool
eqKey (kl :** _) (kr :** _) =
  case testEquality kl kr of
    Nothing   -> False
    Just Refl -> True

-- | "Zip" a single field of a record with a (':**').
--
-- Since we're only operating on a single field, the @n@ type can't vary like
-- in a traditional zip function.
overFragment
  :: Update10 rec
  => (forall a. m a -> n a -> n a) -> Rep10 rec :** m -> rec n -> rec n
overFragment f (k :** x) = overRep10 k (f x)

-- | Newtype used in implementing contravariant conversion of Fragments.  See
-- 'lmapFragment'.  Only exported because it's used in the type of
-- 'lmapFragment', but it can be largely ignored, like the many "ALens" etc.
-- types in "lens".
newtype OpCostar f r a = OpCostar { getOpCostar :: f a -> r }

instance Functor f => Contravariant (OpCostar f r) where
  contramap f (OpCostar g) = OpCostar (g . fmap f)

-- | Simulate a case statement on a (':**') with a record of functions.
--
-- @
--     caseFragment (MyRecord1 (OpCostar isJust) (OpCostar isNothing)) x
-- @
--
-- Is analogous to (pseudo-code):
--
-- @
--     case x of { (_mr1A :** mx) -> isJust mx; (_mr1B :** mx) -> isNothing mx }
-- @
--
-- This is just the action of `Representable10` (whereby @f m@ is isomorphic to
-- @forall a. Rep10 f a -> m a@) plus some newtyping:
--
-- @
--     f (OpCostar m r)                      ~=  (by Representable10)
--     forall a. Rep10 f a -> OpCostar m r a ~=  (by newtype)
--     forall a. Rep10 f a -> f a -> r       ~=  (by GADT constructor)
--     Rep10 f :** m -> r
-- @
caseFragment
  :: Representable10 f
  => f (OpCostar m r)
  -> Rep10 f :** m -> r
caseFragment fco (k :** v) = getOpCostar (fco `index10` k) v

-- | Convert a (':**') to a different key type contravariantly.
--
-- Example usage:
--
--     data MyRecord1 m = MyRecord1 { _mr1A :: Ap10 Int m, _mr1B :: Ap10 Int m }
--     data MyRecord2 m = MyRecord2 { _mr2A :: Ap10 Int m }
--
--     -- Collapse both fields _mr1A and _mr1B onto _mr2A.
--     example
--       :: Rep10 MyRecord1 :** Identity
--       -> Rep10 MyRecord2 :** Identity
--     example = lmapFragment $ \MyRecord2{..} -> MyRecord1
--       { _mr1A = _mr2A
--       , _mr1B = _mr2A
--       }
--
-- It looks weird that the argument converts from @recB@ to @recA@ in order
-- to convert (':**') the other way, so it merits some explanation: first,
-- note that, by @'Representable10' recA@, we know that @recA m@ is
-- isomorphic to @forall a. 'Rep10' recA a -> m a@.  That is, @Rep10 recA@
-- effectively appears in negative position in @recA m@.  So, a function from
-- @recB@ to @recA@ hand-wavingly contains a function in the opposite
-- direction from @Rep10 recA@ to @Rep10 recB@.
--
-- With the intuition out of the way, here's how we actually accomplish the
-- conversion: start off with a record @recB@ where each field is a function
-- that trivially rebuilds the corresponding @(:**)@ in each field with
-- @k :: Rep10 recB@ we literally just put @(k :**)@ with the appropriate
-- newtype constructors.  Then, apply the user's contravariant conversion
-- function, to turn our @recB@ of @recB@-pair-builders into an
-- @recA@ of @recB@-pair-builders.  If the user-provided conversion
-- function involves changing any field types, it must have done so by
-- @contramap@ping the pair-builders: instead of a function that just
-- directly applies @(k :=)@ to its argument, they will now contain functions
-- equivalent to @\ma -> k := _f ma@.  Finally, unpack the @recA@ pair
-- and use its @k@ to fetch that field's @recB@-pair-builder (potentially
-- with a conversion inserted at the front), and apply it to the payload.
--
-- Usage will typically involve applying contramap to some number of fields and
-- leaving the rest unchanged.  If you have a type-changing
-- 'Control.Lens.Setter' at hand, it's probably easier to use
-- 'Data.Ten.Lens.fragmented'.
lmapFragment
  :: forall recA recB m f
   . ( Representable10 recA, Representable10 recB
     , f ~ OpCostar m (Rep10 recB :** m)
     )
  => (recB f -> recA f)
  -> Rep10 recA :** m -> Rep10 recB :** m
lmapFragment f = caseFragment fragmentBuilders
 where
  fragmentBuilders :: recA (OpCostar m (Rep10 recB :** m))
  fragmentBuilders = f (tabulate10 (\k' -> OpCostar (k' :**)))
