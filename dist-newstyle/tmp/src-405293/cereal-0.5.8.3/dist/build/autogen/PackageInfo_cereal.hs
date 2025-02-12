{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_cereal (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "cereal"
version :: Version
version = Version [0,5,8,3] []

synopsis :: String
synopsis = "A binary serialization library"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/GaloisInc/cereal"
