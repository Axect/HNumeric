{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_HNumeric (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,3,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kavis/.cabal/bin"
libdir     = "/home/kavis/.cabal/lib/x86_64-linux-ghc-8.2.2/HNumeric-0.3.0.0-KP8Ka2HvMULJXi3CVngn3B"
dynlibdir  = "/home/kavis/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/kavis/.cabal/share/x86_64-linux-ghc-8.2.2/HNumeric-0.3.0.0"
libexecdir = "/home/kavis/.cabal/libexec/x86_64-linux-ghc-8.2.2/HNumeric-0.3.0.0"
sysconfdir = "/home/kavis/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HNumeric_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HNumeric_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HNumeric_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HNumeric_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HNumeric_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HNumeric_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
