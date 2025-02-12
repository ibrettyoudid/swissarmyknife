{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_os_string (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "os_string"
version :: Version
version = Version [2,0,7] []

synopsis :: String
synopsis = "Library for manipulating Operating system strings."
copyright :: String
copyright = "Julain Ospald 2021-2023"
homepage :: String
homepage = "https://github.com/haskell/os-string/blob/master/README.md"
