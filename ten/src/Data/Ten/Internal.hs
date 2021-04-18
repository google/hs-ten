module Data.Ten.Internal where

import Data.Text (Text)
import qualified Data.Text as T

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
