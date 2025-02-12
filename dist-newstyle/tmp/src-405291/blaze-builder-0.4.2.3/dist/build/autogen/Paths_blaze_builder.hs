{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_blaze_builder (
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
version = Version [0,4,2,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/brett/.cabal/store/ghc-9.4.8/blaze-builder-0.4.2.3-bbbb473ccebcfce7e48fcafd6aa1ee0382e51c487c0cd6e3d1ae6697a572059d/bin"
libdir     = "/home/brett/.cabal/store/ghc-9.4.8/blaze-builder-0.4.2.3-bbbb473ccebcfce7e48fcafd6aa1ee0382e51c487c0cd6e3d1ae6697a572059d/lib"
dynlibdir  = "/home/brett/.cabal/store/ghc-9.4.8/blaze-builder-0.4.2.3-bbbb473ccebcfce7e48fcafd6aa1ee0382e51c487c0cd6e3d1ae6697a572059d/lib"
datadir    = "/home/brett/.cabal/store/ghc-9.4.8/blaze-builder-0.4.2.3-bbbb473ccebcfce7e48fcafd6aa1ee0382e51c487c0cd6e3d1ae6697a572059d/share"
libexecdir = "/home/brett/.cabal/store/ghc-9.4.8/blaze-builder-0.4.2.3-bbbb473ccebcfce7e48fcafd6aa1ee0382e51c487c0cd6e3d1ae6697a572059d/libexec"
sysconfdir = "/home/brett/.cabal/store/ghc-9.4.8/blaze-builder-0.4.2.3-bbbb473ccebcfce7e48fcafd6aa1ee0382e51c487c0cd6e3d1ae6697a572059d/etc"

getBinDir     = catchIO (getEnv "blaze_builder_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "blaze_builder_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "blaze_builder_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "blaze_builder_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "blaze_builder_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "blaze_builder_sysconfdir") (\_ -> return sysconfdir)



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
