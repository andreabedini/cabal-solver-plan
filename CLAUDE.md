# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`cabal-solver-plan` is a single Haskell executable that runs cabal-install's modular dependency solver directly, with every solver input given on the command line instead of coming from a `cabal.project`. These inputs are the compiler, platform, installed package DBs, source repositories, pkg-config DB, constraints, flags and solver settings. It prints the solver's trace to stderr, then either the install plan on stdout or, on stderr, why no plan was found. The plan is text by default, or JSON with `--json`: a graph of units, with the hashes and sizes needed to fetch every package, in the format specified in `docs/plan-format.md`. See `README.md` for example invocations.

The JSON plan is consumed by timbuktu (`../timbuktu`, buck2 rules for cabal packages, `rules/haskell/cabal_install/`). Keep `docs/plan-format.md` in sync with `Plan.hs`; a format change needs a matching change there.

The project is MIT-licensed (`LICENSE`). `README.md` is for users: what the tool is for and what it can't do ("Why"), installing a release binary, and examples. `CONTRIBUTING.md` is for contributors: building, `tests/run.sh`, fourmolu, and the commit convention. When behaviour, options, supported platforms or the release process change, update both; when the commit rules below change, update `CONTRIBUTING.md` too.

## Commands

```sh
cabal build -w ghc-9.14.1                                          # build the exe (9.12.2 also works)
cabal run -w ghc-9.14.1 -- cabal-solver-plan -w ghc-9.14.1 mtl          # run (args after --)
fourmolu -i app                                                    # format (config: fourmolu.yaml)
```

`tests/run.sh BINARY` tests a built executable: `--version`, `--help`, the exit status of usage errors (2), no plan (1) and other errors (3), a `-w` solve, the deprecated `--with-ghc`/`--use-global-packagedb`, and golden JSON plans for `network` and `shake` (`tests/golden/`). The golden solves use `--compiler-id ghc-9.12.2 --platform x86_64-linux` with the package db vendored in `tests/package-db/`, so they give the same plans on every OS. They download Hackage's index into `$CACHE_DIR` (default `_cache`) and read it up to `--index-state 2026-06-01T00:00:00Z`. `UPDATE_GOLDEN=1` rewrites the golden files. The package db is the `*.conf` files of a ghcup GHC 9.12.2's global package db; its unit ids differ from those in `examples/`, which came from another GHC 9.12.2 install. For anything else, run the README examples.

```sh
cabal build -w ghc-9.14.1 && tests/run.sh "$(cabal list-bin -w ghc-9.14.1 cabal-solver-plan)"
```

The acceptance check for the JSON plan is to compare it with cabal-install 3.14.2's plans for `network` and `shake`. These are embedded in `../timbuktu/examples/plan/{network,shake}/BUCK` and were made with GHC 9.12.2 and `--index-state 2026-06-01T00:00:00Z`. Compare by package, version, component, flags and dependency edges; configured units have no unit id by design. Run against a copy of the Hackage index (`--index <copy>/01-index.tar`), never the one in `~/.cache/cabal`.

CI (`.github/workflows/ci.yml`) has these jobs. Released binaries must be exactly the tested ones, so nothing is rebuilt after `build`:
- **build**: builds with GHC 9.12.2 and 9.14.1 (cabal 3.18.1.0) for x86_64/aarch64 Linux, x86_64/aarch64 macOS and x86_64 Windows, runs `cabal test all` (there are no test suites yet), and uploads the stripped `EXECUTABLES` (just `cabal-solver-plan`) in an archive `ARCHIVE_NAME-VERSION-TARGET.tar.gz` (`.zip` on Windows), with VERSION from `VERSION_FROM`; the archives of GHCs other than `RELEASE_GHC` (9.14.1) have `-ghc-GHC` before the extension and are never released. Each archive is uploaded unzipped (`archive: false`), as an artifact named after the file. Linux builds run on `ubuntu-22.04` (glibc 2.35), so the binaries need at most glibc 2.35; 0.1.0.0 needs 2.34 (its newest `GLIBC_` symbol version), as the README says. They link glibc, libgmp and zlib dynamically, deliberately not statically; the job fails on any other `NEEDED` library. macOS binaries may only link system libraries.
- **test**: unpacks the archive on another runner (newer Linux) and runs `tests/run.sh` on it.
- **sdist**: `cabal sdist all`, the source distribution of every package in `cabal.project`. It contains only what the `.cabal` file lists (`extra-doc-files` etc.), not the whole checkout, so a file the package needs must be listed there. It checks that every package has the same version (from the tarball names) and outputs it, and uploads each tarball unzipped, as an artifact named after the file. An unzipped artifact holds one file, so there is one upload step per package, and the job fails if `cabal sdist all` makes more tarballs than that.
- **checksums**: makes `SHA256SUMS` of the source distributions and the `RELEASE_GHC` archives of `sdist`'s version, and uploads it unzipped. It fails if there are no such archives. Every file a release publishes is therefore an artifact of every run, including a manual run (`gh workflow run ci.yml --ref BRANCH`), which never releases.
- **commits** (PRs only): runs git-cliff (`orhun/git-cliff-action`) with `cliff.toml` over the PR's commits, with its changelog filters turned off by `GIT_CLIFF__GIT__*` overrides, so it fails on a commit that is not conventional or whose type no commit parser knows (see below). The overrides cannot go in `cliff.toml`: the history before the first release has unconventional commits.
- **ci-ok**: fails if any of `build`, `test`, `sdist`, `checksums` or `commits` failed or was cancelled. It is the only check `main`'s ruleset requires, so the matrix can change without touching the ruleset. Keep its `if: always()`: without it a failure makes it skipped, which counts as passed.
- **release** (pushes to `main`, in the `release` environment): runs only when the head commit's subject starts with `chore(release): `, and fails unless it is `chore(release): VERSION` with `sdist`'s version. It downloads this run's artifacts, checks them with `sha256sum --check SHA256SUMS`, attests the files that `SHA256SUMS` lists and `SHA256SUMS` itself, creates a draft release, uploads them unchanged, and publishes the release with the matching `CHANGELOG.md` section as notes. Publishing creates the tag `vVERSION`. A rerun continues a draft, or does nothing once the release is published. Releases are immutable.

`main` is protected by a ruleset: no deletion or force push, changes only through PRs merged by rebase (linear history, no approvals needed), and `ci-ok` must pass. Repository admins can bypass it in PRs only. Every action is pinned to a full commit SHA with the version in a comment; Dependabot updates the pins, weekly in one grouped PR, with `ci(deps):` commits so they pass `commits`.

`cabal.project` pins `index-state`; bump it deliberately.

### Commits and releases

Commit messages follow Conventional Commits. The types are `feat`, `fix`, `perf`, `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style` and `revert`; `!` or a `BREAKING CHANGE:` footer marks a breaking change. The allowed types are the commit parsers in `cliff.toml`; to allow a new type, add a parser. The `commits` CI job enforces this; jj runs no git hooks.

Versions follow the PVP with four components, `A.B.C.D` (for example `0.1.0.0`); never turn a version into three components. git-cliff bumps `B.C.D` as SemVer would (`[bump]` in `cliff.toml`): a breaking change bumps `B`, `feat` bumps `C`, `fix`/`perf` bump `D`, and other types make no release; there is no special rule below 1.0. `A` changes only by hand. Tags are `vVERSION`. Without any tag, the first release is `[bump] initial_tag`, `v0.1.0.0`.

The `version:` field in `cabal-solver-plan.cabal` is the only source of the version (the task and the `sdist` job handle every package in `cabal.project`, all with the release's version). Only `mise run release-pr` (`.mise/tasks/release-pr`, a zsh script) and the owner change it and `CHANGELOG.md`; don't edit either otherwise. The task takes the next version from `git cliff --bumped-version` (config: `cliff.toml`), writes it, regenerates `CHANGELOG.md` with git-cliff, and opens a PR titled `chore(release): VERSION`; merging it makes CI's `release` job publish. Don't run the task yourself: it pushes a branch and opens a PR. The release process is described for contributors in `CONTRIBUTING.md`.

The repo is a jj (Jujutsu) colocated repo, so use `jj`, not `git`, for VCS operations.

### Dependency bounds

The bounds in `cabal-solver-plan.cabal` accept the boot packages of both GHC 9.12 and 9.14 (`base ^>= 4.21.0.0 || ^>= 4.22.0.0`, and likewise `containers`, `time`); everything else is `^>=` the latest release, including `Cabal`/`Cabal-syntax`/`cabal-install`/`cabal-install-solver ^>= 3.18.1.0`. Each major Cabal release changes the solver and `Distribution.Simple` internals this tool uses, so bump the four Cabal packages together and fix the compile errors. Do not reach for `allow-newer`: with GHC 9.14.1, `allow-newer: all` picks a `process` that fails to build.

## Architecture

All sources are in `app/`. This tool reaches into cabal-install **internals** (`Distribution.Solver.Modular.*`, `Distribution.Client.*`), which are not a stable API. Most breakage comes from upstream signature changes in those modules.

- **`Options.hs`**: the optparse-applicative CLI, which maps to the `Options` record. It follows clig.dev (see the `cli-guidelines` skill); option names follow cabal-install's where there is one. Design points:
  - `CompilerSource` is either `CompilerFromSystem` (a real `ghc` at a path, `-w`/`--with-compiler`, default `ghc`, loading its global package DB unless `--no-global-package-db`) or `CompilerInline` (a synthetic `CompilerInfo` + `Platform` from `--compiler-id`, `--platform`, etc.). These are alternatives because a global package DB only makes sense when a real GHC exists. The compiler options are parsed as `CompilerOptions` and checked by `toCompilerSource`, which explains a combination that does not go together.
  - `parseOptions` turns every usage error into exit status 2 with a pointer to `--help`. Without a TARGET or `--local-package` it is a usage error that also prints the description and `examples`.
  - Solver settings are `switchPair`s: `--X` on, `--no-X` off, last one wins.
  - `--with-ghc` and `--use-global-packagedb` (timbuktu's `plan.bzl` passes them) are hidden, deprecated aliases: they set `deprecations`, which `Main` prints as warnings.
  - Options are grouped with `parserOptionGroup`; all but the common ones are `hidden` from the usage line. Every option has a metavar and help.
  - Many options parse with Cabal's own `Parsec` instances through the `parsecOption` / `parsecOptionWith` / `parsecArgument` helpers. Reuse them for new options.
  - `--version` prints `cabal-solver-plan VERSION`, with the version from `Paths_cabal_solver_plan`. `tests/run.sh` checks it against the `.cabal` file.
- **`Main.hs`**: wires everything together:
  - `determineCompiler` returns compiler info, platform and the global installed package index.
  - `readPackageDb` reads `*.conf` files straight from a directory, so `--package-db` does not need `ghc-pkg`.
  - `readLocalPackage` reads a `--local-package`. As cabal-install's `basicInstallPolicy` does, local packages shadow index packages with the same id, hide installed ones, are pinned to their version (through `pkgSpecifierConstraints`, in `solve`), and are always targets. A target given with a version range, such as `aeson-2.2.0.0`, is constrained to it the same way.
  - `loadRepositories` handles `--repository`. Unless `--offline` is given, it updates the repository through hackage-security and cabal-install's `withRepoContext'`. Repositories are always treated as secure, can be `file:` URIs, and are cached in `--cache-dir` (default: `cabal-solver-plan` in the XDG cache directory, from `getXdgDirectory XdgCache`). A bare URI gets the auto-generated name `repositoryN`. `--index PATH` skips all of this and uses an index tar as is.
  - Either way, indexes are read by `IndexTar`, never through cabal-install's `01-index.cache`.
  - It then calls `SolverInterface.solve` and drives the resulting `Progress` with `foldProgress` to log the trace, and then the failure (with hints when no index or no installed packages were given) or the plan. With `--json` it passes the result to `Plan`.
  - Exit status: 0 plan, 1 no plan (or a plan `--json` refuses), 2 usage error (`Options`), 3 any other error. `reportError` turns exceptions into a one-line message (no exception type or backtrace); `checkInputs` checks paths and the GHC before the slow work; `inContext` names the file an error is about. Use `fail` for errors in `IO`: `reportError` prints its message as is.
- **`Log.hs`**: messages to the user are a `Message` type logged through a co-log-core `LogAction` (`Logger`), each with a severity. `stderrLogger` filters by the minimum severity (`Info`; `Debug` with `-v`, `Warning` with `-q`) before rendering, so the trace costs nothing unless shown. Add a constructor for a new kind of message rather than printing to stderr. `cabalVerbosity` maps the level to Cabal's `Verbosity` for what Cabal prints itself.
- **`IndexTar.hs`**: reads a Hackage `01-index.tar` directly. It walks the tar headers, seeking over contents, and keeps the entries up to the index-state: every `.cabal` revision, `package.json` and `preferred-versions`. It reads contents lazily (one `.cabal` file per package the solver looks at), and provides cabal-file revision, sha256 and size, and sdist sha256 and size. Walking the about 1 GB index dominates run time (about 4 s).
- **`Plan.hs`**: turns the solver result into units:
  - Upstream `convCP` maps solver results to packages. `makePlan` keeps the `CP QPN`s too, for the qualified names.
  - `finalizePD` with the solver's flags and stanzas decides components.
  - It chooses per-component vs whole-package with cabal-install **3.14**'s rule, deliberately not 3.18's (see `elaborate`).
  - Units have no meaningful ids: in the JSON, `units` is an object keyed by meaningless ids (numbered dependencies first, but the format promises no order), and `packages` is an object keyed by package id. Pre-existing units carry their package-db `unit-id`; configured units carry none, as choosing one is up to the consumer. Every unit lists the solver's qualified names (`showQPN`) for its package; units are not split by qualifier.
  - It prunes to the targets' closure, orders dependencies first, and renders JSON with `aeson-pretty` and a fixed key order.
  - Backpack is not supported. `unsupportedUnits` makes `Main` refuse a plan with a library that has signatures, or an indefinite installed unit, because the instantiated units would be missing.
- **`SolverInterface.hs`**: `solve` is a trimmed re-implementation of cabal-install's solver entry point, built by hand:
  - It converts user constraints and flags into `LabeledPackageConstraint`s, including the setup-`Cabal` min/max version constraints copied from cabal-install (`setupMinCabalVersion`, `setupMaxCabalVersion`).
  - It interprets package preferences (`PreferLatestForSelected`, or `PreferAllOldest` with `--prefer-oldest`).
  - It calls the modular solver's `Solver.solve` directly, not `modularResolver`, because the plan needs the qualified `CP QPN`s and the extra index below. When solving fails it runs `modularResolver` on the same inputs only for its explanation, which reruns the solver as cabal-install does (and honours `--minimize-conflict-set`). With `--with-preinstalled` it prints the bare conflict set instead.
  - It builds the solver `Index` via `convPIs`, then merges in a synthetic index for `--with-preinstalled` packages (`mkExtraIndex`; only lib/exe components are supported). `Main` refuses a `--json` plan that uses one, as no package db holds it.
- **`SetupDeps.hs`**: `applyDefaultSetupDeps` gives Custom and Hooks packages without a `custom-setup` stanza the setup dependencies cabal-install 3.18 assumes. `Main` applies it to every source package before solving. It copies `mkDefaultSetupDeps` and `addCabalDepForHooks`, but works from a `CompilerInfo`, so it also works with `--compiler-id`. Upstream's `setImplicitSetupInfo` and `packageSetupScriptStyle` are reused. `cabalPkgname` is `"Cabal"`; package names are case-sensitive.
- **`SourcePackage/Lens.hs`**: hand-written `Lens'`s (via `Distribution.Compat.Lens`) for `SourcePackage` fields. `SetupDeps` uses them.

The JSON plan has been checked against cabal-install plans for `network`, `shake` (cabal 3.14.2), `entropy` and `aeson-diff` (cabal 3.16.1, made offline with a private `CABAL_DIR`). The only expected difference is that cabal's `plan.json` is unpruned (see `docs/plan-format.md`).

A lot of the code mirrors cabal-install source, including comments that refer to cabal-install functions such as `planPackages` and `addSetupCabalMinVersionConstraint`. When changing behaviour, compare against the matching upstream cabal-install function.

## Style

`fourmolu.yaml`: 4-space indent, leading commas, diff-friendly import lists, no column limit. Default language is `GHC2021` with `-Wall`; qualified imports use the `import X qualified as Y` form.
