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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.Trans.Writer (runWriter, tell)
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics (Generic, Generic1)
import Type.Reflection (TypeRep, Typeable, typeRep)

import Control.Lens.TH (makeLenses)
import Data.Wrapped (Wrapped1(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Data.Portray (Portray)
import Data.Portray.Pretty (prettyShow)

import Data.Ten.HashMap as HM
import Data.Ten
import Data.Ten.Lens ((!=?), (!=))

data InnerRecord f = InnerRecord
  { _irText :: Ap10 Text f
  , _irText2 :: Ap10 Text f
  }
  deriving (Generic, Generic1)
  -- deriving Portray via Wrapped Generic (InnerRecord f)
  deriving
    ( Functor10, Foldable10, Traversable10, Constrained10 c
    , Applicative10, Representable10, Update10, FieldPaths10
    ) via Wrapped1 Generic1 InnerRecord

data ExampleRecord f = ExampleRecord
  { _erInt :: Ap10 Int f
  , _erBool :: Ap10 Bool f
  , _erNest :: InnerRecord f
  }
  deriving (Generic, Generic1)
  -- deriving Portray via Wrapped Generic (ExampleRecord f)
  deriving
    ( Functor10, Foldable10, Traversable10, Constrained10 c
    , Applicative10, Representable10, Update10, FieldPaths10
    ) via Wrapped1 Generic1 ExampleRecord

$(makeLenses 'InnerRecord)
$(makeLenses 'ExampleRecord)

dynProxy :: forall (a :: Type). Typeable a => TypeRep :** Proxy
dynProxy = typeRep @a :** Proxy @a

exampleMap :: HM.HashMap10 (Field10 ExampleRecord) Identity
exampleMap = HM.fromList
  [ erInt !=? 2
  , erBool !=? True
  , erNest.irText !=? "aoeu"
  ]

main :: IO ()
main = defaultMain
  [ testCase "portray empty" $
      prettyShow (HM.fromList @TypeRep @Proxy []) @?= "fromList []"

  , testCase "portray non-empty" $
      prettyShow (HM.fromList [dynProxy @Int, dynProxy @(Int -> Int)]) @?=
        "fromList\n\
        \  [ typeRep @Int :** Proxy, typeRep @(Int -> Int) :** Proxy ]"

  , testCase "portray Identity" $
      prettyShow exampleMap @?=
        "fromList\n\
        \  [ Field10 _erInt :** Identity { runIdentity = 2 }\n\
        \  , Field10 _erBool :** Identity { runIdentity = True }\n\
        \  , Field10 (_irText . _erNest) :** Identity {\
             \ runIdentity = \"aoeu\" }\n\
        \  ]"

  , testCase "length10" $
      getSum @Int
        (foldMap10 (const (Sum 1)) (HM.fromList @TypeRep @Proxy [])) @?= 0

  , testCase "traverse10C" $
      runWriter
        (traverse10C @Portray
          (\ (Identity x) -> [x, x] <$ tell [prettyShow x])
          exampleMap) @?=
      ( HM.fromList
          [ erInt != [2, 2]
          , erBool != [True, True]
          , erNest.irText != ["aoeu", "aoeu"]
          ]
      , ["2", "True", "\"aoeu\""]
      )
  ]
