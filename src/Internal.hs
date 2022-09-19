{-# LANGUAGE CPP #-}

module Internal where

import           Distribution.Simple
#if MIN_VERSION_Cabal(2,4,0)
import           Distribution.Simple.Glob    (matchDirFileGlob)
import           Distribution.Types.PackageDescription
#else
import qualified Distribution.Simple.Utils   as Utils (matchFileGlob)
import           Distribution.PackageDescription
#endif
import           Distribution.Verbosity      (Verbosity)

fromPackageName :: PackageName -> String
#if MIN_VERSION_Cabal(2,0,0)
fromPackageName = unPackageName
#else
fromPackageName (PackageName s) = s
#endif

matchFileGlob :: Verbosity -> PackageDescription -> FilePath -> IO [FilePath]
#if MIN_VERSION_Cabal(2,4,0)
-- | Newer versions of Cabal have removed this function in favour of more configurable implementation
matchFileGlob verb descr = matchDirFileGlob verb (specVersion descr) "."
#else
matchFileGlob _ _ = Utils.matchFileGlob
#endif
