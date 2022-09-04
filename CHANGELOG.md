0.1.40
======

_Andreas Abel, 2022-09-04_

The `hgettext` tool can now parse Haskell files with:
(contributions by Nikola Hadžić)
- `LANGUAGE` extensions [#19](https://github.com/haskell-hvr/hgettext/issues/19)
- CPP directives [#21](https://github.com/haskell-hvr/hgettext/issues/21)

Tested with GHC 7.4 - 8.6.
Requires `Cabal ≤ 2.2`, thus, does not build with GHC 8.8 and above.
