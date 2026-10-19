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

The allowed types are `feat`, `fix`, `perf`, `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style` and `revert`. A scope in parentheses is optional. `!` before the colon, or a `BREAKING CHANGE:` footer, marks a breaking change. The `commits` CI job checks every commit of a pull request with [git-cliff](https://git-cliff.org/), so the allowed types are the ones that the commit parsers in `cliff.toml` know. jj does not run git hooks, so this check is the only one.

The commit types choose the next version:

| Commits since the last release | Next version |
|---|---|
| At least one breaking change, of any type | `B` + 1 |
| At least one `feat` | `C` + 1 |
| At least one `fix` or `perf` | `D` + 1 |
| Only `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style` | No release |

Pull requests are merged with rebase merge. A squash merge would leave only the PR title for the release task to read.

## Versions

Versions follow the [Haskell PVP](https://pvp.haskell.org/) and have four components, `A.B.C.D`, for example `0.1.0.0`. `A.B` is the major version, `C` the minor version and `D` the patch version.

- git-cliff calculates the next version from the commit types, as in the table above. This is SemVer applied to `B.C.D`. There is no special rule for versions below 1.0: a breaking change increments `B` even when it is `0`.
- Incrementing a field sets all fields to its right to `0`.
- `A` changes only by hand, when the maintainer decides that a release deserves a new number. Like a change of `B`, it makes a major release.
- Release tags are `v` followed by the version, for example `v0.1.0.0`. The first release is `0.1.0.0`.
- Every package in `cabal.project` has the version of the release.

The `version:` fields in the `.cabal` files are the only source of the version. Only the release task, and the maintainer in a release PR, change them and `CHANGELOG.md`.

## Making a release

The maintainer makes releases.

1. Make sure that `main` is green.
2. Run `mise run release-pr`. It reads the commits since the last release tag, sets the next version in every `.cabal` file, regenerates `CHANGELOG.md` with [git-cliff](https://git-cliff.org/), and opens a pull request titled `chore(release): VERSION`. If there are no releasable commits, it says so and does nothing.
3. Review the pull request. To increment `A`, change the version by hand in every `.cabal` file, and change the `CHANGELOG.md` header and the commit description to the same version. Edits to `CHANGELOG.md` are lost at the next release, which regenerates it; wording that must stay goes in `cliff.toml`.
4. When `ci-ok` passes, merge the pull request with rebase merge.
5. CI's `release` job sees the release commit on `main`. It publishes a GitHub release with the archives that CI built and tested, the source distribution (`cabal sdist`) of every package in `cabal.project`, their `SHA256SUMS`, and the notes from `CHANGELOG.md`. Publishing creates the tag.
6. Verify the release:

   ```
   gh release verify vVERSION
   gh attestation verify ASSET --repo andreabedini/cabal-solver-plan
   ```

Releases are immutable: a published version cannot change. If a release is wrong, fix it and make a new release. If the `release` job fails before it publishes, rerun the failed jobs; the job continues the draft release.

### Release artifacts without a release

Every CI run, on a pull request or started by hand, uploads each file that a release would publish as an artifact of its own, unzipped and named after the file: the archives built with GHC 9.14.1 (`cabal-solver-plan-VERSION-TARGET.tar.gz`, `.zip` on Windows), the source distribution (`cabal-solver-plan-VERSION.tar.gz`) and `SHA256SUMS`, without attestations. The archives built with other GHCs are artifacts too, with `-ghc-GHC` before the extension, but releases leave them out. To make them for any branch without releasing, start a run and download the files from its summary page:

```
gh workflow run ci.yml --ref BRANCH
gh run list --workflow ci.yml --branch BRANCH --event workflow_dispatch --limit 1
```

The `release` job runs only on a push to `main`, so a manual run never releases.

## Licence

By contributing, you agree that your contributions are licensed under the [MIT licence](LICENSE).
