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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import GHC.Generics (Generic, Generic1)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Sum(..))

import Data.Traversable10
         ( Functor10(..), Foldable10(..), Traversable10
         , Applicative10(..), Constrained10
         , Ap10(..), Pair10(..)
         )
import Data.Vec (Vec, vec3)
import Data.Wrapped (Wrapped1(..))

import Test.HUnit.Lang (assertEqual)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)

data Foo f = Foo
  { x :: Ap10 Word f
  , y :: Ap10 Char f
  , z :: Pair10 Bool Double f
  , w :: Vec 3 (Ap10 Int f)
  }
  deriving (Generic, Generic1)
  deriving
    ( Foldable10, Traversable10, Constrained10 c
    , Functor10, Applicative10
    ) via Wrapped1 Generic1 Foo

deriving instance Show (Foo Maybe)
deriving instance Eq (Foo Maybe)

type BasicFoo = Foo Identity
type MaybeFoo = Foo Maybe
type BoolFoo = Foo (Const Bool)

data Bar f = Bar
  { ordinaryField :: Ap10 Int f
  , nestedField :: Foo f
  }
  deriving (Generic, Generic1)
  deriving
    ( Foldable10, Traversable10, Constrained10 c
    , Functor10, Applicative10
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
  z = Pair10 (Const True) (Const False),
  w = vec3 cTrue cTrue cFalse
 }

fNothing :: Ap10 a Maybe
fNothing = Ap10 Nothing

fJust :: a -> Ap10 a Maybe
fJust a = Ap10 (Just a)

masked :: MaybeFoo
masked = Foo {
  x = fNothing,
  y = fJust 'y',
  z = Pair10 (Just False) Nothing,
  w = vec3 (fJust 2) (fJust 3) fNothing
 }

val :: a -> Ap10 a Identity
val a = Ap10 (Identity a)

basic :: BasicFoo
basic = Foo {
  x = val 123,
  y = val 'y',
  z = Pair10 (Identity False) (Identity 456.0),
  w = vec3 (val 2) (val 3) (val 4)
 }

{- HLINT ignore main "Use list literal" -}
main :: IO ()
main = defaultMain $
  testCase "popcntMask" (
    assertEqual "" 4 (popcntMask theMask)
  ) :
  testCase "applyMask" (
    assertEqual "" masked (applyMask theMask basic)) :
  []
