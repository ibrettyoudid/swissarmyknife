{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tagged (
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
version = Version [0,8,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/brett/.cabal/store/ghc-9.4.8/tagged-0.8.9-a2f5fa70038ef8e87e17f10e8caf2e24dafb54a54a82c1d0867187d56b92483e/bin"
libdir     = "/home/brett/.cabal/store/ghc-9.4.8/tagged-0.8.9-a2f5fa70038ef8e87e17f10e8caf2e24dafb54a54a82c1d0867187d56b92483e/lib"
dynlibdir  = "/home/brett/.cabal/store/ghc-9.4.8/tagged-0.8.9-a2f5fa70038ef8e87e17f10e8caf2e24dafb54a54a82c1d0867187d56b92483e/lib"
datadir    = "/home/brett/.cabal/store/ghc-9.4.8/tagged-0.8.9-a2f5fa70038ef8e87e17f10e8caf2e24dafb54a54a82c1d0867187d56b92483e/share"
libexecdir = "/home/brett/.cabal/store/ghc-9.4.8/tagged-0.8.9-a2f5fa70038ef8e87e17f10e8caf2e24dafb54a54a82c1d0867187d56b92483e/libexec"
sysconfdir = "/home/brett/.cabal/store/ghc-9.4.8/tagged-0.8.9-a2f5fa70038ef8e87e17f10e8caf2e24dafb54a54a82c1d0867187d56b92483e/etc"

getBinDir     = catchIO (getEnv "tagged_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "tagged_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "tagged_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "tagged_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tagged_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tagged_sysconfdir") (\_ -> return sysconfdir)



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
