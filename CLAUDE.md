# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`cabal-solver-plan` is a single Haskell executable that runs cabal-install's modular dependency solver directly, with every solver input given on the command line instead of coming from a `cabal.project`. These inputs are the compiler, platform, installed package DBs, source repositories, pkg-config DB, constraints, flags and solver settings. It prints the solver's trace to stderr, then either the install plan on stdout or the conflict set. The plan is text by default, or JSON with `--json`: units with deterministic ids, hashes and sizes, in the format specified in `docs/plan-format.md`. See `README.md` for example invocations.

The JSON plan is consumed by timbuktu (`../timbuktu`, buck2 rules for cabal packages, `rules/haskell/cabal_install/`). Keep `docs/plan-format.md` in sync with `Plan.hs`; a format change needs a matching change there.

## Commands

```sh
cabal build -w ghc-9.14.1                                          # build the exe (9.12.2 also works)
cabal run -w ghc-9.14.1 -- cabal-solver-plan --with-ghc ghc-9.14.1 mtl   # run (args after --)
fourmolu -i app                                                    # format (config: fourmolu.yaml)
```

There is no test suite. To check that a change works, run the README examples. Use `--use-global-packagedb` to load GHC's global package DB.

The acceptance check for the JSON plan is to compare it with cabal-install 3.14.2's plans for `network` and `shake`. These are embedded in `../timbuktu/examples/plan/{network,shake}/BUCK` and were made with GHC 9.12.2 and `--index-state 2026-06-01T00:00:00Z`. Compare by package, version, component, flags and dependency edges; unit ids differ by design. Run against a copy of the Hackage index (`--index <copy>/01-index.tar`), never the one in `~/.cache/cabal`.

CI (`.github/workflows/ci.yml`) runs `cabal build all` and `cabal test all` with GHC 9.12.2 and 9.14.1, and cabal 3.18.1.0.

The repo is a jj (Jujutsu) colocated repo, so use `jj`, not `git`, for VCS operations.

### Dependency bounds

The bounds in `cabal-solver-plan.cabal` accept the boot packages of both GHC 9.12 and 9.14 (`base ^>= 4.21.0.0 || ^>= 4.22.0.0`, and likewise `containers`, `time`); everything else is `^>=` the latest release, including `Cabal`/`Cabal-syntax`/`cabal-install`/`cabal-install-solver ^>= 3.18.1.0`. Each major Cabal release changes the solver and `Distribution.Simple` internals this tool uses, so bump the four Cabal packages together and fix the compile errors. Do not reach for `allow-newer`: with GHC 9.14.1, `allow-newer: all` picks a `process` that fails to build.

## Architecture

All sources are in `app/`. This tool reaches into cabal-install **internals** (`Distribution.Solver.Modular.*`, `Distribution.Client.*`), which are not a stable API. Most breakage comes from upstream signature changes in those modules.

- **`Options.hs`**: the optparse-applicative CLI, which maps to the `Options` record. Two design points:
  - `CompilerSource` is either `CompilerFromSystem` (a real `ghc` at a path, optionally loading its global package DB) or `CompilerInline` (a synthetic `CompilerInfo` + `Platform` from `--compiler-id`, `--platform`, etc.). These are alternatives because a global package DB only makes sense when a real GHC exists.
  - Many options parse with Cabal's own `Parsec` instances through the `parsecOption` / `parsecOptionWith` / `parsecArgument` helpers. Reuse them for new options.
- **`Main.hs`**: wires everything together:
  - `determineCompiler` returns compiler info, platform and the global installed package index.
  - `readPackageDb` reads `*.conf` files straight from a directory, so `--package-db` does not need `ghc-pkg`.
  - `loadRepositories` handles `--repository`. Unless `--offline` is given, it updates the repository through hackage-security and cabal-install's `withRepoContext'`. Repositories are always treated as secure, can be `file:` URIs, and are cached in `--cache-dir` (default `_cache`). A bare URI gets the auto-generated name `repositoryN`. `--index PATH` skips all of this and uses an index tar as is.
  - Either way, indexes are read by `IndexTar`, never through cabal-install's `01-index.cache`.
  - It then calls `SolverInterface.solve`, and drives the resulting `Progress` with `foldProgress`/`summarizeMessages` to print the trace. With `--json` it passes the result to `Plan`.
- **`IndexTar.hs`**: reads a Hackage `01-index.tar` directly. It walks the tar headers, seeking over contents, and keeps the entries up to the index-state: every `.cabal` revision, `package.json` and `preferred-versions`. It reads contents lazily (one `.cabal` file per package the solver looks at), and provides cabal-file revision, sha256 and size, and sdist sha256 and size. Walking the about 1 GB index dominates run time (about 4 s).
- **`Plan.hs`**: turns the solver result into units:
  - Its own version of `convCP` maps solver results to packages.
  - `finalizePD` with the solver's flags and stanzas decides components.
  - It chooses per-component vs whole-package with cabal-install **3.14**'s rule, deliberately not 3.18's (see `elaborate`).
  - Unit ids hash their dependencies' ids, using a lazily tied `Map`.
  - It prunes to the targets' closure, orders dependencies first, and renders JSON with `aeson-pretty` and a fixed key order.
- **`SolverInterface.hs`**: `solve` is a trimmed re-implementation of cabal-install's solver entry point, built by hand:
  - It converts user constraints and flags into `LabeledPackageConstraint`s, including the setup-`Cabal` min/max version constraints copied from cabal-install (`setupMinCabalVersion`, `setupMaxCabalVersion`).
  - It interprets package preferences (`PreferLatestForSelected`, or `PreferAllOldest` with `--prefer-oldest`).
  - It builds the solver `Index` via `convPIs`, then merges in a synthetic index for `--with-preinstalled` packages (`mkExtraIndex`; only lib/exe components are supported).
- **`SetupDeps.hs`**: `applyDefaultSetupDeps` gives Custom and Hooks packages without a `custom-setup` stanza the setup dependencies cabal-install 3.18 assumes. `Main` applies it to every source package before solving. It copies `mkDefaultSetupDeps` and `addCabalDepForHooks`, but works from a `CompilerInfo`, so it also works with `--compiler-id`. Upstream's `setImplicitSetupInfo` and `packageSetupScriptStyle` are reused. `cabalPkgname` is `"Cabal"`; package names are case-sensitive.
- **`SourcePackage/Lens.hs`**: hand-written `Lens'`s (via `Distribution.Compat.Lens`) for `SourcePackage` fields. `SetupDeps` uses them.

The JSON plan has been checked against cabal-install plans for `network`, `shake` (cabal 3.14.2), `entropy` and `aeson-diff` (cabal 3.16.1, made offline with a private `CABAL_DIR`). The only expected difference is that cabal's `plan.json` is unpruned (see `docs/plan-format.md`).

A lot of the code mirrors cabal-install source, including comments that refer to cabal-install functions such as `planPackages` and `addSetupCabalMinVersionConstraint`. When changing behaviour, compare against the matching upstream cabal-install function.

## Style

`fourmolu.yaml`: 4-space indent, leading commas, diff-friendly import lists, no column limit. Default language is `GHC2021` with `-Wall`; qualified imports use the `import X qualified as Y` form.
