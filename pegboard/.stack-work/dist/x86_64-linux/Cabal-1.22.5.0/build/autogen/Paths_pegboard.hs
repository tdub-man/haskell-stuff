module Paths_pegboard (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/tanner/Documents/LYA-Haskell/pegboard/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/bin"
libdir     = "/home/tanner/Documents/LYA-Haskell/pegboard/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/lib/x86_64-linux-ghc-7.10.3/pegboard-0.1.0.0-FGCQSHOeNg054bfGSPqnnE"
datadir    = "/home/tanner/Documents/LYA-Haskell/pegboard/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/share/x86_64-linux-ghc-7.10.3/pegboard-0.1.0.0"
libexecdir = "/home/tanner/Documents/LYA-Haskell/pegboard/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/libexec"
sysconfdir = "/home/tanner/Documents/LYA-Haskell/pegboard/.stack-work/install/x86_64-linux/lts-6.16/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pegboard_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pegboard_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "pegboard_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pegboard_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pegboard_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
