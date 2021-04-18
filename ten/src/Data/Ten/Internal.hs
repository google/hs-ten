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

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Ten.Internal where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics ((:*:)(..))

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

starFst :: (f :*: g) m -> f m
starFst (f :*: _) = f

starSnd :: (f :*: g) m -> g m
starSnd (_ :*: g) = g
