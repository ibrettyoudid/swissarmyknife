{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_mime_types (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "mime_types"
version :: Version
version = Version [0,1,2,0] []

synopsis :: String
synopsis = "Basic mime-type handling types and functions"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/yesodweb/wai"
