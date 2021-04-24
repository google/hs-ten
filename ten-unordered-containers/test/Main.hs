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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.Trans.Writer (runWriter, tell)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Type.Reflection (TypeRep, Typeable, typeRep)

import Control.Lens.TH (makeLenses)
import Data.Wrapped (Wrapped1(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Data.Portray (Portray)
import Data.Portray.Diff.HUnit ((@?-))
import Data.Portray.Pretty (showPortrayal)

import Data.Ten.HashMap as HM
import Data.Ten
import Data.Ten.Lens ((!=?), (!=))

data InnerRecord f = InnerRecord
  { _irText :: Ap10 Text f
  , _irText2 :: Ap10 Text f
  }
  deriving (Generic, Generic1)
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
  deriving
    ( Functor10, Foldable10, Traversable10, Constrained10 c
    , Applicative10, Representable10, Update10, FieldPaths10
    ) via Wrapped1 Generic1 ExampleRecord
  deriving
    ( Functor10WithIndex, Foldable10WithIndex, Traversable10WithIndex
    ) via Wrapped1 Representable10 ExampleRecord

type instance Index10 ExampleRecord = Field10 ExampleRecord

$(makeLenses 'InnerRecord)
$(makeLenses 'ExampleRecord)

dynProxy :: forall (a :: Type). Typeable a => TypeRep :** Proxy
dynProxy = typeRep @a :** Proxy @a

dyn :: forall a. Typeable a => a -> TypeRep :** Identity
dyn x = typeRep @a :** Identity x

exampleMap :: HM.HashMap10 (Field10 ExampleRecord) Identity
exampleMap = HM.fromList
  [ erInt !=? 2
  , erBool !=? True
  , erNest.irText !=? "aoeu"
  ]

main :: IO ()
main = defaultMain
  [ testCase "portray empty" $
      showPortrayal (HM.fromList @TypeRep @Proxy []) @?= "fromList []"

  , testCase "portray non-empty" $
      showPortrayal (HM.fromList [dynProxy @Int, dynProxy @(Int -> Int)]) @?=
        "fromList\n\
        \  [ typeRep @Int :** Proxy, typeRep @(Int -> Int) :** Proxy ]"

  , testCase "portray Identity" $
      showPortrayal exampleMap @?=
        "fromList\n\
        \  [ Field10 _erInt :** Identity 2\
          \, Field10 _erBool :** Identity True\n\
        \  , Field10 (_irText . _erNest) :** Identity \"aoeu\"\n\
        \  ]"

  , testCase "length10" $
      getSum @Int
        (foldMap10 (const (Sum 1)) (HM.fromList @TypeRep @Proxy [])) @?= 0

  , testCase "traverse10C" $
      runWriter
        (traverse10C @Portray
          (\ (Identity x) -> [x, x] <$ tell [showPortrayal x])
          exampleMap) @?-
      ( HM.fromList
          [ erInt != [2, 2]
          , erBool != [True, True]
          , erNest.irText != ["aoeu", "aoeu"]
          ]
      , ["2", "True", "\"aoeu\""]
      )

  , testCase "toHashMap" $
      ifoldr10 HM.insert HM.empty
        (tabulate10 @Type @ExampleRecord (Const . showPortrayal)) @?-
      HM.fromList
        [ erInt != Const "Field10 _erInt"
        , erBool != Const "Field10 _erBool"
        , erNest.irText != Const "Field10 (_irText . _erNest)"
        , erNest.irText2 != Const "Field10 (_irText2 . _erNest)"
        ]

  , testCase "dyns" $
      HM.lookup (typeRep @Bool)
          (HM.fromList [dyn True, dyn not, dyn (dyn @Int)]) @?=
        Just (Identity True)

  , testCase "map dyns" $
      imap10
        (\ ty (Identity x) -> Const @String $ case ty of
             _
               | Just Refl <- testEquality ty (typeRep @String) -> x
               | Just Refl <- testEquality ty (typeRep @Bool) -> (show x)
               | Just Refl <- testEquality ty (typeRep @(Bool -> Bool))
                   -> "<function>"
               | otherwise -> "<unknown>"
             )
        (HM.fromList [dyn True, dyn @String "hi", dyn not, dyn 'a']) @?-
        (HM.fromList
          [ typeRep @String :** Const "hi"
          , typeRep @Bool :** Const "True"
          , typeRep @(Bool -> Bool) :** Const "<function>"
          , typeRep @Char :** Const "<unknown>"
          ])
  ]
