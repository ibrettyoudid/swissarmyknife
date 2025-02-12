{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_network_uri (
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
version = Version [2,6,4,2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/brett/.cabal/store/ghc-9.4.8/network-uri-2.6.4.2-607d96920ed016e2f668d7641774827fe0bcceb6ac87644056aad8a26e2a18b3/bin"
libdir     = "/home/brett/.cabal/store/ghc-9.4.8/network-uri-2.6.4.2-607d96920ed016e2f668d7641774827fe0bcceb6ac87644056aad8a26e2a18b3/lib"
dynlibdir  = "/home/brett/.cabal/store/ghc-9.4.8/network-uri-2.6.4.2-607d96920ed016e2f668d7641774827fe0bcceb6ac87644056aad8a26e2a18b3/lib"
datadir    = "/home/brett/.cabal/store/ghc-9.4.8/network-uri-2.6.4.2-607d96920ed016e2f668d7641774827fe0bcceb6ac87644056aad8a26e2a18b3/share"
libexecdir = "/home/brett/.cabal/store/ghc-9.4.8/network-uri-2.6.4.2-607d96920ed016e2f668d7641774827fe0bcceb6ac87644056aad8a26e2a18b3/libexec"
sysconfdir = "/home/brett/.cabal/store/ghc-9.4.8/network-uri-2.6.4.2-607d96920ed016e2f668d7641774827fe0bcceb6ac87644056aad8a26e2a18b3/etc"

getBinDir     = catchIO (getEnv "network_uri_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "network_uri_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "network_uri_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "network_uri_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "network_uri_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "network_uri_sysconfdir") (\_ -> return sysconfdir)



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
