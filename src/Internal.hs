{-# LANGUAGE CPP #-}

module Internal where

import           Distribution.Simple

fromPackageName :: PackageName -> String
#if MIN_VERSION_Cabal(2,0,0)
fromPackageName = unPackageName
#else
fromPackageName (PackageName s) = s
#endif
