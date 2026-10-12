# cabal-solver-plan

Run cabal-install's dependency solver with every input given on the command line, and print the build plan: no `cabal.project` and no cabal-install configuration.

The inputs are:
- the compiler (a real `ghc`, or just its id with `--compiler-id`) and platform;
- package dbs;
- package indexes;
- local packages;
- constraints, flags and solver settings.

The solver trace goes to stderr. The plan goes to stdout, as text, or as JSON with `--json` (see [docs/plan-format.md](docs/plan-format.md)).

Build it with GHC 9.12 or 9.14:

```
cabal build -w ghc-9.14.1
```

Run `cabal-solver-plan --help` for all options.

## Examples

Without a package index, the only packages are the ones GHC comes with (`--use-global-packagedb`):

```
❯ cabal-solver-plan --with-ghc ghc-9.12.2 --use-global-packagedb mtl
[__0] trying: mtl-2.3.1/installed-6d75 (user goal)
[__1] trying: base-4.21.0.0/installed-b7a5 (dependency of mtl)
…
[__6] done
rts-1.0.2/installed-1.0.2

ghc-prim-0.13.0/installed-00de
lib	rts-1.0.2/installed-1.0.2
…
```

Download (or update) Hackage's index into `--cache-dir` (default `_cache`) and solve with it:

```
cabal-solver-plan --with-ghc ghc-9.12.2 --use-global-packagedb \
  --repository https://hackage.haskell.org/ aeson
```

Use a repository again without contacting the network:

```
cabal-solver-plan --with-ghc ghc-9.12.2 --use-global-packagedb \
  --repository https://hackage.haskell.org/ --offline aeson
```

Solve from an index tar alone, at a fixed index-state, and print the plan as JSON. The plan includes the hashes, sizes and `.cabal` revisions needed to fetch every package, all read from the index:

```
cabal-solver-plan --with-ghc ghc-9.12.2 --use-global-packagedb \
  --index ~/index-copy/01-index.tar --index-state 2026-06-01T00:00:00Z \
  --json shake > shake.json
```

See [examples/](examples/) for the resulting plans.

Plan for a local package (a directory with a `.cabal` file, as in `cabal.project`'s `packages:`):

```
cabal-solver-plan --with-ghc ghc-9.12.2 --use-global-packagedb \
  --index ~/index-copy/01-index.tar --local-package ./my-package --json
```

Without a compiler, give its id and platform instead of `--with-ghc`:

```
cabal-solver-plan --compiler-id ghc-9.12.2 --platform x86_64-linux \
  --index ~/index-copy/01-index.tar --json text
```

With `--compiler-id` there are no pre-installed packages, so the solver will want to build `base` and other boot packages from source, which usually fails. Use `--package-db` (a directory of `.conf` files) or `--with-preinstalled PKGID[:LIB]` to tell it what is installed.
