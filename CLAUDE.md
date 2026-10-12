# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`cabal-solver-plan` is a single Haskell executable that runs cabal-install's modular dependency solver directly, with every solver input given on the command line instead of coming from a `cabal.project`. These inputs are the compiler, platform, installed package DBs, source repositories, pkg-config DB, constraints, flags and solver settings. It prints the solver's trace and then either the install plan (one block per package, listing deps by component) or the conflict set. See `README.md` for example invocations and output.

## Commands

```sh
cabal build -w ghc-9.14.1                                          # build the exe
cabal run -w ghc-9.14.1 -- cabal-solver-plan --with-ghc ghc-9.14.1 mtl   # run (args after --)
fourmolu -i app                                                    # format (config: fourmolu.yaml)
```

There is no test suite. To check that a change works, run the README examples. Use `--use-global-packagedb` to load GHC's global package DB. A `--repository` URI needs a trailing slash (`https://hackage.haskell.org/`).

CI (`.github/workflows/ci.yml`) runs `cabal build all` and `cabal test all` with GHC 9.14.1 and cabal 3.18.1.0.

The repo is a jj (Jujutsu) colocated repo, so use `jj`, not `git`, for VCS operations.

### Dependency bounds

The bounds in `cabal-solver-plan.cabal` are `^>=` on the latest versions that build with GHC 9.14.1: `base ^>= 4.22` and `Cabal`/`Cabal-syntax`/`cabal-install`/`cabal-install-solver ^>= 3.18.1.0`. Each major Cabal release changes the solver and `Distribution.Simple` internals this tool uses, so bump the four Cabal packages together and fix the compile errors. Do not reach for `allow-newer`: with GHC 9.14.1, `allow-newer: all` picks a `process` that fails to build.

## Architecture

All sources are in `app/`. This tool reaches into cabal-install **internals** (`Distribution.Solver.Modular.*`, `Distribution.Client.*`), which are not a stable API. Most breakage comes from upstream signature changes in those modules.

- **`Options.hs`**: the optparse-applicative CLI, which maps to the `Options` record. Two design points:
  - `CompilerSource` is either `CompilerFromSystem` (a real `ghc` at a path, optionally loading its global package DB) or `CompilerInline` (a synthetic `CompilerInfo` + `Platform` from `--compiler-id`, `--platform`, etc.). These are alternatives because a global package DB only makes sense when a real GHC exists.
  - Many options parse with Cabal's own `Parsec` instances through the `parsecOption` / `parsecOptionWith` / `parsecArgument` helpers. Reuse them for new options.
- **`Main.hs`**: wires everything together:
  - `determineCompiler` returns compiler info, platform and the global installed package index.
  - `readPackageDb` reads `*.conf` files straight from a directory, so `--package-db` does not need `ghc-pkg`.
  - `loadRepositories` uses hackage-security plus cabal-install's `withRepoContext'` / `getSourcePackagesAtIndexState`. Repositories are always treated as secure, can be `file:` URIs, are cached in `--cache-dir` (default `_cache`), and are refreshed unless `--offline` is given. A bare URI gets the auto-generated name `repositoryN`.
  - It then calls `SolverInterface.solve`, and drives the resulting `Progress` with `foldProgress`/`showMessages` to print the trace and the result.
- **`SolverInterface.hs`**: `solve` is a trimmed re-implementation of cabal-install's solver entry point, built by hand:
  - It converts user constraints and flags into `LabeledPackageConstraint`s, including the setup-`Cabal` min/max version constraints copied from cabal-install (`setupMinCabalVersion`, `setupMaxCabalVersion`).
  - It interprets package preferences (`PreferLatestForSelected`, or `PreferAllOldest` with `--prefer-oldest`).
  - It builds the solver `Index` via `convPIs`, then merges in a synthetic index for `--with-preinstalled` packages (`mkExtraIndex`; only lib/exe components are supported).
- **`SetupDeps.hs`**: code copied from cabal-install's `ProjectPlanning` for default `custom-setup` dependencies. Only `cabalPkgname` is used at present; `applyDefaultSetupDeps` is not wired in.
- **`SourcePackage/Lens.hs`**: hand-written `Lens'`s (via `Distribution.Compat.Lens`) for `SourcePackage` fields. `SetupDeps` uses them.

A lot of the code mirrors cabal-install source, including comments that refer to cabal-install functions such as `planPackages` and `addSetupCabalMinVersionConstraint`. When changing behaviour, compare against the matching upstream cabal-install function.

## Style

`fourmolu.yaml`: 4-space indent, leading commas, diff-friendly import lists, no column limit. Default language is `GHC2021` with `-Wall`; qualified imports use the `import X qualified as Y` form.
