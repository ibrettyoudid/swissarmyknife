{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_primitive (
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
version = Version [0,9,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/brett/.cabal/store/ghc-9.4.8/primitive-0.9.0.0-c17bfbc8573696bac8dd2ab2fd37a6595b395851b9dacccaf9812eafc03386b4/bin"
libdir     = "/home/brett/.cabal/store/ghc-9.4.8/primitive-0.9.0.0-c17bfbc8573696bac8dd2ab2fd37a6595b395851b9dacccaf9812eafc03386b4/lib"
dynlibdir  = "/home/brett/.cabal/store/ghc-9.4.8/primitive-0.9.0.0-c17bfbc8573696bac8dd2ab2fd37a6595b395851b9dacccaf9812eafc03386b4/lib"
datadir    = "/home/brett/.cabal/store/ghc-9.4.8/primitive-0.9.0.0-c17bfbc8573696bac8dd2ab2fd37a6595b395851b9dacccaf9812eafc03386b4/share"
libexecdir = "/home/brett/.cabal/store/ghc-9.4.8/primitive-0.9.0.0-c17bfbc8573696bac8dd2ab2fd37a6595b395851b9dacccaf9812eafc03386b4/libexec"
sysconfdir = "/home/brett/.cabal/store/ghc-9.4.8/primitive-0.9.0.0-c17bfbc8573696bac8dd2ab2fd37a6595b395851b9dacccaf9812eafc03386b4/etc"

getBinDir     = catchIO (getEnv "primitive_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "primitive_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "primitive_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "primitive_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "primitive_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "primitive_sysconfdir") (\_ -> return sysconfdir)



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
