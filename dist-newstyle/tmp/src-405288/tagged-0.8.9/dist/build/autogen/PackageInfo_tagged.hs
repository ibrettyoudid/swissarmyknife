{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_tagged (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "tagged"
version :: Version
version = Version [0,8,9] []

synopsis :: String
synopsis = "Haskell 98 phantom types to avoid unsafely passing dummy arguments"
copyright :: String
copyright = "2009-2015 Edward A. Kmett"
homepage :: String
homepage = "http://github.com/ekmett/tagged"
