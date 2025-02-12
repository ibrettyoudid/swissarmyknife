{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_primitive (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "primitive"
version :: Version
version = Version [0,9,0,0] []

synopsis :: String
synopsis = "Primitive memory-related operations"
copyright :: String
copyright = "(c) Roman Leshchinskiy 2009-2012"
homepage :: String
homepage = "https://github.com/haskell/primitive"
