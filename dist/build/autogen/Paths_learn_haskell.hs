module Paths_learn_haskell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/junix/.cabal/bin"
libdir     = "/Users/junix/.cabal/lib/x86_64-osx-ghc-7.10.2/learn-haskell-1.0-L3QDp1tZ3XPGSRuozimhA5"
datadir    = "/Users/junix/.cabal/share/x86_64-osx-ghc-7.10.2/learn-haskell-1.0"
libexecdir = "/Users/junix/.cabal/libexec"
sysconfdir = "/Users/junix/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "learn_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "learn_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "learn_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "learn_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "learn_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
