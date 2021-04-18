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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Field
         ( Field(..)
         , FieldRep(..), GFieldPaths(..), GTabulate(..)
         ) where

import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import GHC.Generics
         ( Generic1(..)
         , (:*:)(..), (:.:)(..)
         , M1(..), Rec1(..), U1(..), Par1(..)
         , Meta(..), S, C, D
         )
import GHC.TypeLits (KnownSymbol, symbolVal)

import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep, collectRep)
import Data.Wrapped (Wrapped1(..))
import Text.PrettyPrint.HughesPJ (text)
import Text.PrettyPrint.HughesPJClass (Pretty(..))

import Data.Ten.Internal
         ( PathComponent(..), dropUnderscore, showsPath, starFst, starSnd
         )

-- | A 'Rep' type in the form of a parametric accessor function.
newtype Field f = Field { getField :: forall a. f a -> a }

-- | Build a record where each field has a description of the field's location.
--
-- This primarily powers the 'Show' and 'Pretty' instances of 'Field'.
class FieldPaths f where
  fieldPaths :: f [PathComponent]

instance FieldPaths f => Show (Field f) where
  showsPrec p (Field f) = showParen (p > 10) $
    showString "Field " . showsPath 11 (coerce $ f fieldPaths)

instance FieldPaths f => Pretty (Field f) where
  pPrintPrec _ p f = text (showsPrec (round p) f "")

instance (Generic1 rec, GFieldPaths (Rep1 rec))
      => FieldPaths (Wrapped1 Generic1 rec) where
  fieldPaths = Wrapped1 . to1 $ gfieldPaths id
  {-# INLINE fieldPaths #-}

-- | The 'Generic1' implementation of 'FieldPaths'.
--
-- As with 'GTabulate', derive this only to enable using your type as a
-- sub-record; otherwise just derive 'FieldPaths' directly.
class GFieldPaths rec where
  gfieldPaths :: ([PathComponent] -> r) -> rec r

instance GFieldPaths U1 where
  gfieldPaths _ = U1
  {-# INLINE gfieldPaths #-}

instance GFieldPaths Par1 where
  gfieldPaths r = Par1 $ r []
  {-# INLINE gfieldPaths #-}

instance GFieldPaths rec => GFieldPaths (Rec1 rec) where
  gfieldPaths = Rec1 . gfieldPaths
  {-# INLINE gfieldPaths #-}

instance GFieldPaths rec => GFieldPaths (M1 C i rec) where
  gfieldPaths = M1 . gfieldPaths
  {-# INLINE gfieldPaths #-}

-- Non-newtype constructors: wait until we get to the fields to assign a path
-- component.
instance GFieldPaths rec
      => GFieldPaths (M1 D ('MetaData n m p 'False) rec) where
  gfieldPaths = M1 . gfieldPaths
  {-# INLINE gfieldPaths #-}

-- Newtype constructors: immediately decide to use 'NewtypeIso'.
instance GFieldPaths rec
      => GFieldPaths
           (M1 D ('MetaData n m p 'True) (M1 C i (M1 S j rec))) where
  gfieldPaths r = M1 . M1 . M1 $ gfieldPaths (r . (NewtypeIso:))
  {-# INLINE gfieldPaths #-}

instance (KnownSymbol sym, GFieldPaths rec)
      => GFieldPaths (M1 S ('MetaSel ('Just sym) b c d) rec) where
  gfieldPaths r = M1 . gfieldPaths $
    r . (NamedField (T.pack nm) (T.pack $ dropUnderscore nm) :)
   where
    nm = symbolVal @sym Proxy
  {-# INLINE gfieldPaths #-}

instance (GFieldPaths f, GFieldPaths g) => GFieldPaths (f :*: g) where
  gfieldPaths r = gfieldPaths r :*: gfieldPaths r
  {-# INLINE gfieldPaths #-}

instance (GFieldPaths f, GFieldPaths g) => GFieldPaths (f :.: g) where
  gfieldPaths r = Comp1 $
    gfieldPaths $ \outer ->
    gfieldPaths $ \inner ->
    r $ outer ++ inner
  {-# INLINE gfieldPaths #-}

-- | The 'Generic1' implementation of 'tabulate' for 'Field'.
class GTabulate rec where
  gtabulate :: (Field rec -> r) -> rec r

newtype FieldRep f a = FieldRep (f a)
  deriving Functor

-- Only to satisfy the superclass constraint of Representable.
instance (Generic1 f, GTabulate (Rep1 f), Functor f)
      => Distributive (FieldRep f) where
  distribute = distributeRep
  collect = collectRep

instance (Generic1 f, GTabulate (Rep1 f), Functor f)
      => Applicative (FieldRep f) where
  pure x = tabulate (const x)
  f <*> x = tabulate (index f <*> index x)

instance (Generic1 f, GTabulate (Rep1 f), Functor f)
      => Monad (FieldRep f) where
  x >>= f = tabulate (index x >>= index . f)

instance (Generic1 f, GTabulate (Rep1 f), Functor f)
      => Representable (FieldRep f) where
  type Rep (FieldRep f) = Field f
  index (FieldRep f) (Field g) = g f
  tabulate f = FieldRep $ to1 $ gtabulate $ \i -> f $ Field $ getField i . from1

instance GTabulate U1 where
  gtabulate _ = U1
  {-# INLINE gtabulate #-}

instance GTabulate rec => GTabulate (Rec1 rec) where
  gtabulate r = Rec1 $ gtabulate (coerce r)
  {-# INLINE gtabulate #-}

instance GTabulate rec => GTabulate (M1 k i rec) where
  gtabulate r = M1 $ gtabulate (coerce r)
  {-# INLINE gtabulate #-}

instance GTabulate Par1 where
  gtabulate r = Par1 $ r (Field coerce)
  {-# INLINE gtabulate #-}

instance (GTabulate f, GTabulate g) => GTabulate (f :*: g) where
  gtabulate r = ftab :*: gtab
   where
    ftab = gtabulate $ \ (Field g) -> r $ Field $ g . starFst
    gtab = gtabulate $ \ (Field g) -> r $ Field $ g . starSnd
  {-# INLINE gtabulate #-}

instance (GTabulate f, GTabulate g) => GTabulate (f :.: g) where
  gtabulate r = Comp1 $
    gtabulate $ \ (Field g0) ->
    gtabulate $ \ (Field g1) ->
    r (Field $ g1 . g0 . unComp1)
  {-# INLINE gtabulate #-}

{-
instance KnownNat n => GTabulate (Vec n) where
  gtabulate r = tabulate $ \i -> r $ Field (Vec.! i)
  {-# INLINE gtabulate #-}
  -}
