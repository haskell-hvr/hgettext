## 0.1.40.2

_Nikola Hadžić, 2025-01-13_

- Library binding for [`bind_textdomain_codeset`](https://man.archlinux.org/man/core/gettext/bind_textdomain_codeset.3.en) function ([#26](https://github.com/haskell-hvr/hgettext/pull/26), contribution by Rafał Kuźnia)

Tested with GHC 8.0.2 - 9.10.1.

## 0.1.40.1

_Andreas Abel, 2022-09-19_

- Allow `Cabal ≥ 2.4` ([#15](https://github.com/haskell-hvr/hgettext/issues/15), thanks Claudio Bley!)

Tested with GHC 7.4 - 9.4.

## 0.1.40

_Andreas Abel, 2022-09-04_

The `hgettext` tool can now parse Haskell files with:
(contributions by Nikola Hadžić)
- `LANGUAGE` extensions [#19](https://github.com/haskell-hvr/hgettext/issues/19)
- CPP directives [#21](https://github.com/haskell-hvr/hgettext/issues/21)

Tested with GHC 7.4 - 8.6.
Requires `Cabal ≤ 2.2`, thus, does not build with GHC 8.8 and above.
