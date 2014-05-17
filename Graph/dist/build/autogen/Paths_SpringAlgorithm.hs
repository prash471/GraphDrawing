module Paths_SpringAlgorithm (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/prashant/.cabal/bin"
libdir     = "/home/prashant/.cabal/lib/SpringAlgorithm-0.1.0.0/ghc-7.4.1"
datadir    = "/home/prashant/.cabal/share/SpringAlgorithm-0.1.0.0"
libexecdir = "/home/prashant/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "SpringAlgorithm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SpringAlgorithm_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "SpringAlgorithm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SpringAlgorithm_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
