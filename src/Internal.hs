{-# LANGUAGE CPP #-}

module Internal (fromPackageName, matchFileGlob) where

import           Distribution.Simple
#if MIN_VERSION_Cabal(2,4,0)
import           Distribution.Simple.Glob    (matchDirFileGlob)
import           Distribution.Verbosity      (silent)
#else
import           Distribution.Simple.Utils   (matchFileGlob)
#endif

fromPackageName :: PackageName -> String
#if MIN_VERSION_Cabal(2,0,0)
fromPackageName = unPackageName
#else
fromPackageName (PackageName s) = s
#endif

#if MIN_VERSION_Cabal(2,4,0)
-- | Newer versions of Cabal have removed this function in favour of more configurable implementation
-- We assume Cabal 2.0
matchFileGlob :: FilePath -> IO [FilePath]
matchFileGlob = matchDirFileGlob silent (mkVersion [2, 0]) "."
#endif
