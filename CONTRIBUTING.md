# Contributing

Bug reports and pull requests are welcome.

## Reporting a bug

Please include:
- the output of `cabal-solver-plan --version`;
- the full command you ran;
- what it printed on stderr, with `-v` so that it includes the solver's trace;
- what you expected instead. If cabal-install gives a different plan, say which version of cabal-install, and how you ran it.

## Building

You need GHC 9.12 or 9.14, and a recent cabal (CI uses 3.18.1.0):

```
cabal build -w ghc-9.14.1
```

## Testing

`tests/run.sh` tests a built executable:

```
tests/run.sh "$(cabal list-bin -w ghc-9.14.1 cabal-solver-plan)"
```

It needs a `ghc` on the `PATH` (or set `GHC`). The first run downloads Hackage's index, about 1.2 GB, into `_cache` in the repository (or `$CACHE_DIR`).

The golden tests compare JSON plans with `tests/golden/`. If you change the output on purpose, run the tests with `UPDATE_GOLDEN=1` to rewrite the golden files, and check the diff. A change to the JSON format also needs a matching change to [docs/plan-format.md](docs/plan-format.md) and [examples/](examples/).

Besides the golden plans, it checks the exit status of usage errors (2), of no plan (1) and of other errors (3). CI builds and tests every pull request on Linux, macOS and Windows.

## Code style

Format the code with [fourmolu](https://github.com/fourmolu/fourmolu), which uses the repository's `fourmolu.yaml`:

```
fourmolu -i app
```

## Commit messages

Commit messages follow [Conventional Commits](https://www.conventionalcommits.org/). For example:

```
fix: honour --offline for file: repositories
feat: add --enable-tests
feat!: rename the "db" field of pre-existing units
docs: explain --with-preinstalled
```

Releases are automated with release-please. It uses the commit types to choose the next version and to write `CHANGELOG.md`: `fix:` makes a patch release, `feat:` a minor one, and `!` a major one (before 1.0, `feat:` and `!` both make a minor release). Other types, such as `docs:`, `ci:`, `test:`, `refactor:` and `chore:`, don't make a release on their own. Don't edit the version in `cabal-solver-plan.cabal` or `CHANGELOG.md` by hand.

## Licence

By contributing, you agree that your contributions are licensed under the [MIT licence](LICENSE).
