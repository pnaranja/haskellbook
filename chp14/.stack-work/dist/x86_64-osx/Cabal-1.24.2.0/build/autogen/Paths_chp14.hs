{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_chp14 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/paul/Haskell/haskellbook/chp14/.stack-work/install/x86_64-osx/lts-8.24/8.0.2/bin"
libdir     = "/Users/paul/Haskell/haskellbook/chp14/.stack-work/install/x86_64-osx/lts-8.24/8.0.2/lib/x86_64-osx-ghc-8.0.2/chp14-0.1.0.0"
dynlibdir  = "/Users/paul/Haskell/haskellbook/chp14/.stack-work/install/x86_64-osx/lts-8.24/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/paul/Haskell/haskellbook/chp14/.stack-work/install/x86_64-osx/lts-8.24/8.0.2/share/x86_64-osx-ghc-8.0.2/chp14-0.1.0.0"
libexecdir = "/Users/paul/Haskell/haskellbook/chp14/.stack-work/install/x86_64-osx/lts-8.24/8.0.2/libexec"
sysconfdir = "/Users/paul/Haskell/haskellbook/chp14/.stack-work/install/x86_64-osx/lts-8.24/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chp14_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chp14_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chp14_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chp14_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chp14_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chp14_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
