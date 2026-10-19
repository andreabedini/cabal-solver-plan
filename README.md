# cabal-solver-plan

Run cabal-install's dependency solver with every input given on the command line, and print the build plan: no `cabal.project` and no cabal-install configuration.

The inputs are:
- the compiler (a real `ghc`, or just its id with `--compiler-id`) and platform;
- package dbs;
- package indexes;
- local packages;
- constraints, flags and solver settings.

The plan goes to stdout, as text, or as JSON with `--json` (see [docs/plan-format.md](docs/plan-format.md)). Progress, errors and, with `-v`, the solver's trace go to stderr.

## Why

`cabal build --dry-run` also finds a build plan, but its inputs are spread out: `cabal.project`, `~/.config/cabal/config`, the package index in `~/.cache/cabal`, whichever `ghc` is on the `PATH`, and the network. `cabal-solver-plan` takes every input from the command line, so a plan depends only on what you give it. That makes it useful for:

- **Reproducible plans.** Read the index from a file with `--index` and pin it with `--index-state`. The same command then gives the same plan on any machine, without network access or cabal configuration.
- **Plans for another machine.** With `--compiler-id`, `--platform` and `--package-db` you can plan for a GHC you don't have installed, or for another OS or architecture. For example, you can make an aarch64-linux plan on a macOS laptop.
- **Other build systems.** The JSON plan lists every unit to build, with its component, flags and dependencies. For each package it also has the sha256 and size of the source tarball, and the revision, sha256 and size of the `.cabal` file. With that, a build system other than cabal-install (Buck2, Bazel, Nix, …) can fetch and build exactly what the solver chose, without running cabal.
- **Solver experiments and bug reports.** You see the solver's full trace. You can try a constraint, a flag, another compiler, or `--prefer-oldest` (to test lower bounds) without editing a project file. And you can turn a solver problem into a single command that anyone can run.

It runs cabal-install's own solver, from the cabal-install 3.18 library, with cabal-install's defaults. Its JSON plans have been checked against cabal-install's for several packages.

It does not:
- build anything, or download source packages;
- plan test suites or benchmarks;
- plan Backpack packages (`--json` refuses them);
- use repositories that aren't secure (hackage-security) repositories.

## Install

### Pre-built binaries

Download the archive for your platform from the [latest release](https://github.com/andreabedini/cabal-solver-plan/releases/latest) and unpack it. It contains the `cabal-solver-plan` executable. There are archives for x86_64 and aarch64 Linux, x86_64 and aarch64 macOS, and x86_64 Windows.

- Linux binaries need glibc 2.34 or newer (e.g. Ubuntu 22.04, Debian 12, RHEL 9), libgmp and zlib.
- macOS binaries are not notarised. Remove the quarantine attribute before running one: `xattr -d com.apple.quarantine cabal-solver-plan`.

Each release also has `SHA256SUMS`, and a GitHub attestation for every file. To check an archive:

```
sha256sum --check --ignore-missing SHA256SUMS
gh attestation verify ARCHIVE --repo andreabedini/cabal-solver-plan
```

### With mise

[mise](https://mise.jdx.dev/) installs the release archive for your platform with its [GitHub backend](https://mise.jdx.dev/dev-tools/backends/github.html), and checks its checksum and attestation:

```
mise use -g github:andreabedini/cabal-solver-plan
```

Add `@VERSION` to pin a version, for example `github:andreabedini/cabal-solver-plan@0.1.0.0`. Without `-g`, the tool is added to the current project's `mise.toml`.

### From source

Each release has the source distribution, `cabal-solver-plan-VERSION.tar.gz`. With GHC 9.12 or 9.14 and cabal, unpack it and run, in its directory:

```
cabal install
```

Run `cabal-solver-plan --help` for all options.

## Usage

```
cabal-solver-plan [OPTIONS] TARGET...
```

The TARGETs are packages to plan for, such as `aeson`, or `aeson-2.2.0.0` for that version. By default the compiler is the `ghc` on the `PATH` (or `-w PATH`), with its global package db. `cabal-solver-plan --help` lists all the options, in groups, with examples.

The plan goes to stdout. Progress and errors go to stderr: `-v` adds the solver's trace, and `-q` leaves out everything but warnings and errors.

The exit status is:
- 0: a plan was found;
- 1: no plan was found (the solver says why);
- 2: usage error;
- 3: any other error, e.g. a file that cannot be read.

## Examples

Without a package index, the only packages are the ones installed with GHC:

```
❯ cabal-solver-plan -w ghc-9.12.2 mtl
rts-1.0.2/installed-1.0.2

ghc-prim-0.13.0/installed-ca1e
lib	rts-1.0.2/installed-1.0.2
…
```

Download (or update) Hackage's index into the cache directory, and solve with it. The cache directory is `cabal-solver-plan` in `$XDG_CACHE_HOME` (usually `~/.cache`), or `%LOCALAPPDATA%` on Windows; `--cache-dir` changes it.

```
cabal-solver-plan --repository hackage.haskell.org:https://hackage.haskell.org/ aeson
```

Use the repository again without contacting the network:

```
cabal-solver-plan --repository hackage.haskell.org:https://hackage.haskell.org/ --offline aeson
```

Solve from an index tar alone, at a fixed index-state, and print the plan as JSON. The plan includes the hashes, sizes and `.cabal` revisions needed to fetch every package, all read from the index:

```
cabal-solver-plan -w ghc-9.12.2 \
  --index ~/index-copy/01-index.tar --index-state 2026-06-01T00:00:00Z \
  --json shake > shake.json
```

See [examples/](examples/) for the resulting plans.

Plan for a local package (a directory with a `.cabal` file, as in `cabal.project`'s `packages:`):

```
cabal-solver-plan -w ghc-9.12.2 \
  --index ~/index-copy/01-index.tar --local-package ./my-package --json
```

Without a compiler, give its id and platform instead of `-w`:

```
cabal-solver-plan --compiler-id ghc-9.12.2 --platform x86_64-linux \
  --package-db ghc-9.12.2-db --index ~/index-copy/01-index.tar --json text
```

With `--compiler-id` there are no installed packages unless you give them, and the solver cannot build `base` and the other boot packages from source. Give the compiler's package db with `--package-db` (a directory of `.conf` files), or name installed packages with `--with-preinstalled PKGID[:LIB]`. `--json` only accepts packages from `--package-db`.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Licence

MIT. See [LICENSE](LICENSE).
