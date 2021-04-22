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

-- | Provides 'Generic1' derivation of @Representable10@ based on 'Field10'.
--
-- Like with "Data.Functor.Field", we use parametric functions
-- @forall m. f m -> m a@ to identify positions tagged with type @a@ within
-- @f@.  This leads to instances for 'Data.Ten.Representable.Representable10'
-- and 'Data.Ten.Update.Update10'.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Field (Field10(..), FieldPaths10(..), GFieldPaths10(..)) where

import Control.Monad.Trans.State (state, evalState)
import Data.Coerce (coerce)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.Type.Equality (TestEquality(..))
import GHC.Generics
         ( Generic1(..)
         , (:*:)(..), (:.:)(..)
         , M1(..), Rec1(..), U1(..)
         , Meta(..), S, C, D
         )
import GHC.TypeLits (KnownSymbol, symbolVal)

import Data.Hashable (Hashable(..))
import Data.Portray (Portray(..), Portrayal(..))
import Data.Wrapped (Wrapped1(..))

import Data.Functor.Field (GFieldPaths(..))
import Data.Ten.Ap (Ap10(..))
import Data.Ten.Applicative (Applicative10(..))
import Data.Ten.Internal
         ( PathComponent(..), dropUnderscore, showsPath, portrayPath
         )
import Data.Ten.Traversable (Traversable10, fsequenceA10)
import {-# SOURCE #-} Data.Ten.Update (Update10, EqualityTable(..), equalityTable)

-- | A 'Data.Ten.Representable.Rep10' type as a parametric accessor function.
newtype Field10 f a = Field10 { getField10 :: forall m. f m -> m a }

instance Update10 f => TestEquality (Field10 f) where
  testEquality (Field10 f) (Field10 g) = case f equalityTable of
    EqualityTable tbl -> unComp1 (g tbl)

fieldNumbers :: (Traversable10 f, Applicative10 f) => f (Const Int)
fieldNumbers =
  flip evalState 0 $
  fsequenceA10 (pure10 $ Comp1 $ state $ \i -> (Const i, i + 1))

instance (Traversable10 f, Applicative10 f) => Eq (Field10 f a) where
  Field10 x == Field10 y = x fieldNumbers == y fieldNumbers

instance (Traversable10 f, Applicative10 f) => Ord (Field10 f a) where
  Field10 x `compare` Field10 y = x fieldNumbers `compare` y fieldNumbers

instance (Traversable10 f, Applicative10 f) => Hashable (Field10 f a) where
  hashWithSalt salt (Field10 x) = hashWithSalt salt $ x fieldNumbers

instance FieldPaths10 f => Show (Field10 f a) where
  showsPrec p (Field10 f) = showParen (p > 10) $
    showString "Field10 " . showsPath 11 (coerce $ f fieldPaths10)

instance FieldPaths10 f => Portray (Field10 f a) where
  portray (Field10 f) = Apply "Field10" [portrayPath $ coerce $ f fieldPaths10]

-- | Provides a path of field selectors / lenses identifying each "field".
class FieldPaths10 (rec :: (k -> Type) -> Type) where
  fieldPaths10 :: rec (Const [PathComponent])

instance (Generic1 rec, GFieldPaths10 (Rep1 rec))
      => FieldPaths10 (Wrapped1 Generic1 rec) where
  fieldPaths10 = Wrapped1 . to1 $ gfieldPaths10 Const
  {-# INLINE fieldPaths10 #-}

-- | 'Generic1' implementation of 'FieldPaths10'.
class GFieldPaths10 (rec :: (k -> Type) -> Type) where
  gfieldPaths10 :: (forall a. [PathComponent] -> r a) -> rec r

instance GFieldPaths10 U1 where
  gfieldPaths10 _ = U1
  {-# INLINE gfieldPaths10 #-}

instance GFieldPaths10 (Ap10 a) where
  gfieldPaths10 r = Ap10 $ r []
  {-# INLINE gfieldPaths10 #-}

instance GFieldPaths10 rec => GFieldPaths10 (Rec1 rec) where
  gfieldPaths10 r = Rec1 $ gfieldPaths10 r
  {-# INLINE gfieldPaths10 #-}

instance GFieldPaths10 rec => GFieldPaths10 (M1 C i rec) where
  gfieldPaths10 r = M1 $ gfieldPaths10 r
  {-# INLINE gfieldPaths10 #-}

-- Non-newtype constructors: wait until we get to the fields to assign a path
-- component.
instance GFieldPaths10 rec => GFieldPaths10 (M1 D ('MetaData n m p 'False) rec) where
  gfieldPaths10 r = M1 $ gfieldPaths10 r
  {-# INLINE gfieldPaths10 #-}

-- Newtype constructors: immediately decide to use 'NewtypeIso'.
instance GFieldPaths10 rec
      => GFieldPaths10 (M1 D ('MetaData n m p 'True) (M1 C i (M1 S j rec))) where
  gfieldPaths10 r = M1 . M1 . M1 $ gfieldPaths10 (r . (NewtypeIso:))
  {-# INLINE gfieldPaths10 #-}

instance (KnownSymbol sym, GFieldPaths10 rec)
      => GFieldPaths10 (M1 S ('MetaSel ('Just sym) b c d) rec) where
  gfieldPaths10 r = M1 $ gfieldPaths10 $
    r . (NamedField (T.pack nm) (T.pack $ dropUnderscore nm) :)
   where
    nm = symbolVal @sym Proxy
  {-# INLINE gfieldPaths10 #-}

instance (GFieldPaths10 f, GFieldPaths10 g) => GFieldPaths10 (f :*: g) where
  gfieldPaths10 r = gfieldPaths10 r :*: gfieldPaths10 r
  {-# INLINE gfieldPaths10 #-}

instance (GFieldPaths f, GFieldPaths10 g) => GFieldPaths10 (f :.: g) where
  gfieldPaths10 r = Comp1 $
    gfieldPaths $ \outer ->
    gfieldPaths10 $ \inner ->
    r $ outer ++ inner
  {-# INLINE gfieldPaths10 #-}
