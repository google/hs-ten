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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.DeepSeq (NFData(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Sum(..))
import GHC.Generics (Generic, Generic1)

import Data.Default.Class (Default(..))
import Data.Distributive (Distributive(..))
import Data.Functor.Field (FieldRep(..))
import Data.Functor.Update (Update)
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Portray (Portray)
import Data.Portray.Diff (Diff)
import Data.Ten
         ( Functor10(..), Foldable10(..), Traversable10
         , Applicative10(..), Constrained10, Representable10, Update10
         , Ap10(..)
         )
import Data.Wrapped (Wrapped1(..), Wrapped(..))

import Test.HUnit.Lang (assertEqual)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)

data Pair a = Pair a a
  deriving
    ( Functor, Foldable, Traversable
    , Eq, Ord, Read, Show, Default, NFData
    , Generic, Generic1
    )
  deriving (Representable, Update, Applicative, Monad) via FieldRep Pair
  deriving (Portray, Diff) via Wrapped Generic (Pair a)

instance Distributive Pair where distribute = distributeRep

data Foo f = Foo
  { x :: Ap10 Word f
  , y :: Ap10 String f
  , z :: Ap10 Double f
  , w :: Pair (Ap10 Int f)
  }
  deriving (Eq, Ord, Read, Show, Default, NFData, Generic, Generic1)
  deriving (Portray, Diff) via Wrapped Generic (Foo f)
  deriving
    ( Foldable10, Traversable10, Constrained10 c
    , Functor10, Applicative10, Representable10, Update10
    ) via Wrapped1 Generic1 Foo

type BasicFoo = Foo Identity
type MaybeFoo = Foo Maybe
type BoolFoo = Foo (Const Bool)

data Bar f = Bar
  { ordinaryField :: Ap10 Int f
  , nestedField :: Foo f
  }
  deriving (Eq, Ord, Read, Show, Default, NFData, Generic, Generic1)
  deriving (Portray, Diff) via Wrapped Generic (Bar f)
  deriving
    ( Foldable10, Traversable10, Constrained10 c
    , Functor10, Applicative10, Representable10, Update10
    ) via Wrapped1 Generic1 Bar

justAll :: BasicFoo -> MaybeFoo
justAll = fmap10 (Just . runIdentity)

applyMask :: BoolFoo -> BasicFoo -> MaybeFoo
applyMask =
  liftA210 (\(Const b) (Identity a) -> if b then Just a else Nothing)

popcntMask :: BoolFoo -> Int
popcntMask =
  getSum . foldMap10 (\(Const b) -> Sum (if b then 1 else 0))

--

cFalse, cTrue :: Ap10 a (Const Bool)
cFalse = Ap10 (Const False)
cTrue = Ap10 (Const True)

theMask :: BoolFoo
theMask = Foo {
  x = cFalse,
  y = cTrue,
  z = cTrue,
  w = Pair cTrue cFalse
 }

fNothing :: Ap10 a Maybe
fNothing = Ap10 Nothing

fJust :: a -> Ap10 a Maybe
fJust a = Ap10 (Just a)

masked :: MaybeFoo
masked = Foo {
  x = fNothing,
  y = fJust "y",
  z = Ap10 (Just 456.0),
  w = Pair (fJust 6) fNothing
 }

val :: a -> Ap10 a Identity
val a = Ap10 (Identity a)

basic :: BasicFoo
basic = Foo {
  x = val 123,
  y = val "y",
  z = val 456.0,
  w = Pair (val 6) (val 7)
 }

{- HLINT ignore main "Use list literal" -}
main :: IO ()
main = defaultMain $
  testCase "popcntMask" (
    assertEqual "" 3 (popcntMask theMask)
  ) :
  testCase "applyMask" (
    assertEqual "" masked (applyMask theMask basic)) :
  []
