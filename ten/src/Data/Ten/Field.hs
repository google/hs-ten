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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ten.Field (Field10(..), FieldPaths10(..)) where

import Data.Coerce (coerce)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
         ( Generic1(..)
         , (:*:)(..)
         , M1(..), Rec1(..), U1(..)
         , Meta(..), S, C, D
         )
import GHC.TypeLits (KnownSymbol, symbolVal)

import Data.Wrapped (Wrapped1(..))
import Text.PrettyPrint.HughesPJ (text)
import Text.PrettyPrint.HughesPJClass (Pretty(..))

import Data.Ten.Ap (Ap10(..))

data PathComponent
  = NewtypeIso
    -- ^ Zooming in on the contents of a newtype with 'coerce' or '_Wrapped'.
  | NamedField !Text !Text
    -- ^ Zooming in on a record field with the given named selector/lens.

showPathComponent :: PathComponent -> ShowS
showPathComponent NewtypeIso = showString "coerce"
showPathComponent (NamedField selectorName _lensName) =
  showString (T.unpack selectorName)

showsPath :: Int -> [PathComponent] -> ShowS
showsPath p path = case reverse path of
  -- If the path ends up empty, that means either there's a bug, or we've added
  -- support to GHC for a new Generics representation type equivalent to Ap10,
  -- and we're looking at it as a standalone GFieldPaths00 instance.  Since
  -- that'll be a newtype, we'll represent it as "coerce", since that should
  -- work regardless of what it ends up being called.
  []     -> showString "coerce"
  [x]    -> showPathComponent x
  (x:xs) -> showParen (p > 9) $
    showPathComponent x .
    flip (foldr (\y -> showString " . " . showPathComponent y)) xs

-- Guess the name of the lens corresponding to a field.
dropUnderscore :: String -> String
dropUnderscore ('_':x) = x
dropUnderscore x = x

-- | A 'Rep10' type in the form of a parametric accessor function.
newtype Field10 f a = Field10 { getField10 :: forall m. f m -> m a }

instance FieldPaths10 rec => Show (Field10 rec a) where
  showsPrec p (Field10 f) = showParen (p > 10) $
    showString "Field10 " . showsPath 11 (coerce $ f fieldPaths10)

instance FieldPaths10 rec => Pretty (Field10 rec a) where
  -- TODO(awpr): it'd be nice to make this configurable between lens-style and
  -- Field10-constructor-and-function style, there's not really a good
  -- configuration mechanism to use for it.
  pPrintPrec _ p f = text (showsPrec (round p) f "")
class FieldPaths10 (rec :: (k -> Type) -> Type) where
  fieldPaths10 :: rec (Const [PathComponent])

instance (Generic1 rec, GFieldPaths10 (Rep1 rec))
      => FieldPaths10 (Wrapped1 Generic1 rec) where
  fieldPaths10 = Wrapped1 . to1 $ gfieldPaths10 Const
  {-# INLINE fieldPaths10 #-}

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

{-
instance (GFieldPaths00 f, GFieldPaths10 g) => GFieldPaths10 (f :.: g) where
  gfieldPaths10 r = Comp1 $
    gfieldPaths00 $ \outer ->
    gfieldPaths10 $ \inner ->
    r $ outer ++ inner
  {-# INLINE gfieldPaths10 #-}
-}