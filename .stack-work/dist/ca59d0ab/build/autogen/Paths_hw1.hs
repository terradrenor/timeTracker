{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hw1 (
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

bindir     = "C:\\Users\\1\\timeTracker\\.stack-work\\install\\7abedeba\\bin"
libdir     = "C:\\Users\\1\\timeTracker\\.stack-work\\install\\7abedeba\\lib\\x86_64-windows-ghc-8.0.2\\hw1-0.1.0.0-MINIKH6kpTCK6ZiYi3d4J"
dynlibdir  = "C:\\Users\\1\\timeTracker\\.stack-work\\install\\7abedeba\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\1\\timeTracker\\.stack-work\\install\\7abedeba\\share\\x86_64-windows-ghc-8.0.2\\hw1-0.1.0.0"
libexecdir = "C:\\Users\\1\\timeTracker\\.stack-work\\install\\7abedeba\\libexec"
sysconfdir = "C:\\Users\\1\\timeTracker\\.stack-work\\install\\7abedeba\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
