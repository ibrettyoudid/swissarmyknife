{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_blaze_builder (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "blaze_builder"
version :: Version
version = Version [0,4,2,3] []

synopsis :: String
synopsis = "Efficient buffered output."
copyright :: String
copyright = "(c) 2010-2014 Simon Meier\n(c) 2010 Jasper Van der Jeugt\n(c) 2013-2015 Leon P Smith"
homepage :: String
homepage = "https://github.com/blaze-builder/blaze-builder"
