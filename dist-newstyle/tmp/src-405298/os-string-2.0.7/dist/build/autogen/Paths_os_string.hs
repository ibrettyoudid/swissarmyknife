{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_os_string (
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
version = Version [2,0,7] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/brett/.cabal/store/ghc-9.4.8/os-string-2.0.7-8675535ad48da29cce8b8c3a6b04cfe164323da78865cc076f18abf5f29bd9a2/bin"
libdir     = "/home/brett/.cabal/store/ghc-9.4.8/os-string-2.0.7-8675535ad48da29cce8b8c3a6b04cfe164323da78865cc076f18abf5f29bd9a2/lib"
dynlibdir  = "/home/brett/.cabal/store/ghc-9.4.8/os-string-2.0.7-8675535ad48da29cce8b8c3a6b04cfe164323da78865cc076f18abf5f29bd9a2/lib"
datadir    = "/home/brett/.cabal/store/ghc-9.4.8/os-string-2.0.7-8675535ad48da29cce8b8c3a6b04cfe164323da78865cc076f18abf5f29bd9a2/share"
libexecdir = "/home/brett/.cabal/store/ghc-9.4.8/os-string-2.0.7-8675535ad48da29cce8b8c3a6b04cfe164323da78865cc076f18abf5f29bd9a2/libexec"
sysconfdir = "/home/brett/.cabal/store/ghc-9.4.8/os-string-2.0.7-8675535ad48da29cce8b8c3a6b04cfe164323da78865cc076f18abf5f29bd9a2/etc"

getBinDir     = catchIO (getEnv "os_string_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "os_string_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "os_string_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "os_string_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "os_string_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "os_string_sysconfdir") (\_ -> return sysconfdir)



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
