{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_mime_types (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,2,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/brett/.cabal/store/ghc-9.4.8/mime-types-0.1.2.0-779c66631d5c5f83585c0bb9da6963b2d85404a462c3d275a714c54c463e29a4/bin"
libdir     = "/home/brett/.cabal/store/ghc-9.4.8/mime-types-0.1.2.0-779c66631d5c5f83585c0bb9da6963b2d85404a462c3d275a714c54c463e29a4/lib"
dynlibdir  = "/home/brett/.cabal/store/ghc-9.4.8/mime-types-0.1.2.0-779c66631d5c5f83585c0bb9da6963b2d85404a462c3d275a714c54c463e29a4/lib"
datadir    = "/home/brett/.cabal/store/ghc-9.4.8/mime-types-0.1.2.0-779c66631d5c5f83585c0bb9da6963b2d85404a462c3d275a714c54c463e29a4/share"
libexecdir = "/home/brett/.cabal/store/ghc-9.4.8/mime-types-0.1.2.0-779c66631d5c5f83585c0bb9da6963b2d85404a462c3d275a714c54c463e29a4/libexec"
sysconfdir = "/home/brett/.cabal/store/ghc-9.4.8/mime-types-0.1.2.0-779c66631d5c5f83585c0bb9da6963b2d85404a462c3d275a714c54c463e29a4/etc"

getBinDir     = catchIO (getEnv "mime_types_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "mime_types_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "mime_types_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "mime_types_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mime_types_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mime_types_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
