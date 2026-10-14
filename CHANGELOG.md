# Changelog

## 0.1.0 (2026-10-14)


### ⚠ BREAKING CHANGES

* errors other than "no plan" and usage errors exit with 3, not 1.
* --cache-dir defaults to the user's cache directory, not _cache. Pass --cache-dir _cache to use an existing download.
* --reorder-goals, --count-conflicts, --fine-grained-conflicts, --minimize-conflict-set, --independent-goals, --avoid-reinstalls, --strong-flags and --solve-executables no longer take an argument; use --no-X to turn one off. --shadow-pkgs is renamed --shadow-installed-packages. --prune-after-first-success is removed.
* usage errors exit with 2, not 1, and a TARGET or --local-package is required.
* the global package db is loaded by default; use --no-global-package-db for the old behaviour. --with-ghc and --use-global-packagedb are deprecated.
* the solver's trace is printed only with --verbose. --verbosity is removed.

### Features

* add --version ([8f5b9f8](https://github.com/andreabedini/cabal-solver-plan/commit/8f5b9f8836cfd08141aff11a7d2430da208f83ea))
* exit with 2 on usage errors, and explain how to run the tool without a target ([e8369a7](https://github.com/andreabedini/cabal-solver-plan/commit/e8369a78e2ec4a09f55553c8a1c572ff2b08b1f6))
* keep downloaded repositories in the user's cache directory ([2e1c776](https://github.com/andreabedini/cabal-solver-plan/commit/2e1c7769f797a5acab486bb3e22037434b271a31))
* load the global package db by default, and take cabal's -w/--with-compiler ([b882ae3](https://github.com/andreabedini/cabal-solver-plan/commit/b882ae30a7c6e2caf21828e39e6615e9fc73296b))
* organise --help, and explain every option ([9ec5c41](https://github.com/andreabedini/cabal-solver-plan/commit/9ec5c41d6768b2dd00b0d6f42ed5ada130329cdd))
* print the solver's trace only with --verbose ([1d2fef9](https://github.com/andreabedini/cabal-solver-plan/commit/1d2fef9e0825f6519cd56f651c36f51b09059da1))
* report errors for humans, exit with 3 on them, and hint at missing inputs ([ee2c4f5](https://github.com/andreabedini/cabal-solver-plan/commit/ee2c4f5fab04e9d634384bb426668cc01ea332fa))
* turn solver settings into --X/--no-X switches named as in cabal-install ([f9a8537](https://github.com/andreabedini/cabal-solver-plan/commit/f9a8537ffd4009538ffd4f249401e68e54bf0306))


### Bug Fixes

* read package indexes on Windows ([af082ce](https://github.com/andreabedini/cabal-solver-plan/commit/af082cea041eada18f416398dcc84b5c994767cd))
* reject --use-global-packagedb with --compiler-id or --no-global-package-db ([95d050c](https://github.com/andreabedini/cabal-solver-plan/commit/95d050cbd67e9b31663133e92752f6176c0b01be))
