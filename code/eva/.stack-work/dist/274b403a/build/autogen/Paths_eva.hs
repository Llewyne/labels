{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_eva (
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

bindir     = "C:\\Users\\Eva\\Documents\\thesis\\labels\\code\\eva\\.stack-work\\install\\a75b1cd5\\bin"
libdir     = "C:\\Users\\Eva\\Documents\\thesis\\labels\\code\\eva\\.stack-work\\install\\a75b1cd5\\lib\\x86_64-windows-ghc-8.10.3\\eva-0.1.0.0-7LpzJZVbmUrAHM5lpjNHcp"
dynlibdir  = "C:\\Users\\Eva\\Documents\\thesis\\labels\\code\\eva\\.stack-work\\install\\a75b1cd5\\lib\\x86_64-windows-ghc-8.10.3"
datadir    = "C:\\Users\\Eva\\Documents\\thesis\\labels\\code\\eva\\.stack-work\\install\\a75b1cd5\\share\\x86_64-windows-ghc-8.10.3\\eva-0.1.0.0"
libexecdir = "C:\\Users\\Eva\\Documents\\thesis\\labels\\code\\eva\\.stack-work\\install\\a75b1cd5\\libexec\\x86_64-windows-ghc-8.10.3\\eva-0.1.0.0"
sysconfdir = "C:\\Users\\Eva\\Documents\\thesis\\labels\\code\\eva\\.stack-work\\install\\a75b1cd5\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "eva_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "eva_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "eva_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "eva_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "eva_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "eva_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
