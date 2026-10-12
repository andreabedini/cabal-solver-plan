# Plan format

`cabal-solver-plan --json` prints a build plan in this format on stdout. Everything else (index loading, the solver trace) goes to stderr.

The output is pretty-printed with a fixed key order, so the same inputs give byte-identical output.

## Top level

```json
{
  "format": 1,
  "compiler-id": "ghc-9.12.2",
  "os": "linux",
  "arch": "x86_64",
  "index-state": { "hackage.haskell.org": "2026-05-31T23:08:56Z" },
  "targets": ["shake-0.19.9-…", "shake-0.19.9-e-shake-…"],
  "packages": [ … ],
  "units": [ … ]
}
```

| Key | Meaning |
|---|---|
| `format` | Version of this format. Currently `1`. |
| `compiler-id`, `os`, `arch` | The compiler and platform the plan is for. |
| `index-state` | For each package index, the timestamp of the newest entry used. Passing it back as `--index-state` reproduces the plan. |
| `targets` | The ids of the units that make up the requested targets. |
| `packages` | The source packages in the plan, one entry per package version. |
| `units` | The units to build or use, dependencies before the units that need them. |

## Packages

```json
{
  "id": "network-3.2.8.0",
  "name": "network",
  "version": "3.2.8.0",
  "build-type": "Configure",
  "src": {
    "type": "repo-tar",
    "repo": "https://hackage.haskell.org/",
    "sha256": "51788e5e…",
    "size": 146148
  },
  "cabal-file": {
    "revision": 0,
    "sha256": "c6f02f26…",
    "size": 5305
  }
}
```

- `build-type` is the package's `build-type` (`Simple`, `Configure`, `Custom` or `Hooks`).
- `src.repo` is the repository URI. It always ends in `/`, so the tarball is at `<repo>package/<id>.tar.gz`.
- `src.sha256` and `src.size` describe that tarball. They come from the package's `package.json` in the index (hackage-security target metadata), and are `null` if the index has none.
- A local package (`--local-package`) has `"src": {"type": "local", "path": "<dir>"}`, with the directory as given, and `"cabal-file": null`.
- `cabal-file` is the revision of the `.cabal` file the plan was made with. It can be downloaded from `<repo>package/<id>/revision/<revision>.cabal`. Revision 0 is the file uploaded with the package, and each later index entry for the file is a new revision; only entries up to the index-state count.

## Units

A unit is either a pre-existing library from a package db, or something built from a source package.

### Pre-existing units

```json
{
  "id": "base-4.21.0.0-b7a5",
  "type": "pre-existing",
  "name": "base",
  "version": "4.21.0.0",
  "component": "lib",
  "db": "global",
  "depends": ["ghc-internal-9.1202.0-7700", "ghc-prim-0.13.0-00de"]
}
```

- `id` is the installed unit id from the package db.
- `component` is `lib` for a main library, or `lib:<name>` for a sublibrary.
- `db` is `global` for the compiler's global package db, or the path given with `--package-db`.

### Configured units

```json
{
  "id": "hsc2hs-0.68.10-e-hsc2hs-71059c12…",
  "type": "configured",
  "package": "hsc2hs-0.68.10",
  "name": "hsc2hs",
  "version": "0.68.10",
  "build-style": "per-component",
  "component": "exe:hsc2hs",
  "flags": { "in-ghc-tree": false },
  "depends": ["base-4.21.0.0-b7a5", "…"],
  "exe-depends": [],
  "setup-depends": []
}
```

- `package` is the id of the unit's entry in `packages`.
- `flags` lists every flag of the package with its value, defaults included.
- `depends` are the units of the libraries the unit builds against. A dependency on a sublibrary is the id of a unit whose `component` is `lib:<name>`.
- `exe-depends` are the units of the executables it needs at build time (`build-tool-depends`, and known `build-tools`).
- `setup-depends` are the libraries the package's `Setup.hs` needs. Only whole-package units have any.

`build-style` says how the unit is built, following cabal-install 3.14:

- **`per-component`**: the unit is one component, named in `component` (`lib`, `lib:<name>`, `flib:<name>`, `exe:<name>`, `test:<name>` or `bench:<name>`). Packages are built this way unless one of the conditions below applies.
- **`whole-package`**: the unit is the whole package, built with its `Setup.hs`. `components` lists the components it builds, and `depends` and `exe-depends` are the union over them, without dependencies between the package's own components. This applies when the build-type is `Configure`, `Custom` or `Hooks`, when the `cabal-version` is older than 1.8, or when the package has no buildable components. (cabal-install 3.18 builds `Configure` packages per component; this tool does not.)

A target package gets all of its buildable libraries, foreign libraries and executables. Other packages only get the components something in the plan needs. Test suites and benchmarks are only included when the solver enabled them.

This is a difference from cabal-install's `plan.json`, which is written before pruning to the targets: it also lists library components that nothing needs. One example is the internal library `vector:lib:benchmarks-O2`, which only vector's benchmarks use, together with its dependencies such as `tasty`.

### Unit ids

The id of a configured unit is `<name>-<version>`, then a component suffix (none for `lib`; `-l-<name>`, `-f-<name>`, `-e-<name>`, `-t-<name>` or `-b-<name>` otherwise), then `-` and a sha256. The sha256 covers:
- the package id, component and flags;
- the compiler and platform;
- the sha256s of the `.cabal` file and tarball;
- the ids of all dependencies.

The ids look like cabal-install's, but the hashes differ from cabal-install's.

The sources of a local package can change at any time, so its units get fixed ids, as in cabal-install: `<name>-<version>-inplace` plus the component suffix, for example `demo-0.1.0.0-inplace-e-demo`. Local packages are always targets.
