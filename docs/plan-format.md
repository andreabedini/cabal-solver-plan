# Plan format

`cabal-solver-plan --json` prints a build plan in this format on stdout. Everything else (progress, errors and, with `-v`, the solver's trace) goes to stderr.

The output is pretty-printed with a fixed key order, so the same inputs give byte-identical output.

## Top level

```json
{
  "format": 3,
  "compiler-id": "ghc-9.12.2",
  "os": "linux",
  "arch": "x86_64",
  "index-state": { "hackage.haskell.org": "2026-05-31T23:08:56Z" },
  "targets": ["38", "39"],
  "packages": { … },
  "units": { … }
}
```

| Key | Meaning |
|---|---|
| `format` | Version of this format. Currently `3`. |
| `compiler-id`, `os`, `arch` | The compiler and platform the plan is for. |
| `index-state` | For each package index, the timestamp of the newest entry used. Passing it back as `--index-state` reproduces the plan. |
| `targets` | The ids of the units that make up the requested targets. |
| `packages` | The source packages in the plan, one entry per package version, keyed by package id (`<name>-<version>`). |
| `units` | The units to build or use, keyed by unit id. |

## Packages

```json
"network-3.2.8.0": {
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

- The key is the package id, `<name>-<version>`. Configured units refer to their package by it.
- `build-type` is the package's `build-type` (`Simple`, `Configure`, `Custom` or `Hooks`).
- `src.repo` is the repository URI. It always ends in `/`, so the tarball is at `<repo>package/<name>-<version>.tar.gz`.
- `src.sha256` and `src.size` describe that tarball. They come from the package's `package.json` in the index (hackage-security target metadata), and are `null` if the index has none.
- A local package (`--local-package`) has `"src": {"type": "local", "path": "<dir>"}`, with the directory as given, and `"cabal-file": null`.
- `cabal-file` is the revision of the `.cabal` file the plan was made with. It can be downloaded from `<repo>package/<name>-<version>/revision/<revision>.cabal`. Revision 0 is the file uploaded with the package, and each later index entry for the file is a new revision; only entries up to the index-state count.

## Units

A unit is either a pre-existing library from a package db, or something built from a source package.

The plan is a graph whose nodes are the units. A unit's id is its key in `units`, and it is only used to refer to the unit from `targets` and from other units' `depends`, `exe-depends` and `setup-depends`. It means nothing else: the same unit can get a different id in another plan, and it is not a unit id that GHC or cabal-install would use. Choosing unit ids for configured units is up to whoever builds the plan.

Neither the ids nor the order of `units` say anything about dependencies. The graph has no cycles, so to build dependencies first, follow the edges.

Every unit has `qualified-names`, the solver's qualified names for the package it comes from, sorted. The solver chooses the dependencies of a package's `Setup.hs`, and of each build tool, separately from the rest of the plan, and qualifies their names to keep them apart:

- `<name>` is a target, or a package the targets' libraries need, directly or not.
- `<pkg>:setup.<name>` is a package that the `Setup.hs` of `<pkg>` needs, directly or not.
- `<pkg>:<tool>:exe.<name>` is a package that the build tool `<tool>` of `<pkg>` needs, `<tool>` included.
- `<pkg>.base` is the `base` that `<pkg>` needs, when the installed `base` itself depends on `base` (the base-3 compatibility shim of GHC 7.x). It does not happen with newer GHCs.

With `--independent-goals`, each name also starts with `<target>.`, for the target whose dependencies it is part of.

Several qualified names can choose the same package version (or installed unit). The solver allows only one of them to make that choice by itself, and makes the others link to it, which means the same flags, stanzas and dependencies (its "single instance restriction"). So the plan has one unit for them, as cabal-install does, and the unit lists every name. In the example below, `base` serves both `network` and `hsc2hs`, the build tool `network` uses. Every unit of a source package gets all of the package's names.

Each qualified name belongs to one package version, so the `qualified-names` of two units of the same package and component never overlap, and a qualified name with a component names exactly one unit (for a whole-package unit, the component is one of its `components`).

This restricts what qualified names can differ in. They can choose different versions of a package, and then the plan has a unit for each version. They cannot choose the same version with different flags or stanzas, even though the units could be built separately; cabal-install's solver rejects that too. For example, with a `Custom` package `bar` that needs `splitmix` in its library and its `Setup.hs`:

- `--constraint 'splitmix ==0.1.3.2' --constraint 'bar:setup.splitmix ==0.1.3.1'` gives two units, `splitmix-0.1.3.2` (`splitmix`) and `splitmix-0.1.3.1` (`bar:setup.splitmix`).
- `--constraint 'splitmix -optimised-mixer' --constraint 'bar:setup.splitmix +optimised-mixer'` cannot use 0.1.3.2 for both. The solver gives the setup `splitmix-0.1.3.1 +optimised-mixer` instead, and with only one version available there would be no plan.

The plan checks this: if the qualified names it merges ever had different flags, stanzas or dependencies, `cabal-solver-plan --json` stops with an error instead of keeping one of them.

### Pre-existing units

```json
"4": {
  "type": "pre-existing",
  "name": "base",
  "version": "4.21.0.0",
  "qualified-names": ["base", "network:hsc2hs:exe.base"],
  "component": "lib",
  "db": "global",
  "unit-id": "base-4.21.0.0-b7a5",
  "depends": ["3", "1"]
}
```

- `name` and `version` are those of the unit's package. A pre-existing unit has no entry in `packages`, so it gives them itself.
- `unit-id` is the unit's id in its package db. It is fixed by the package db, so it has to be used as is.
- `component` is `lib` for a main library, or `lib:<name>` for a sublibrary.
- `db` is `global` for the compiler's global package db, or the path given with `--package-db`.

### Configured units

```json
"23": {
  "type": "configured",
  "package": "hsc2hs-0.68.10",
  "qualified-names": ["network:hsc2hs:exe.hsc2hs"],
  "build-style": "per-component",
  "component": "exe:hsc2hs",
  "flags": { "in-ghc-tree": false },
  "depends": ["4", "21", "20", "16", "22"],
  "exe-depends": [],
  "setup-depends": []
}
```

- `package` is the key of the unit's entry in `packages`, which has its name and version.
- `flags` lists every flag of the package with its value, defaults included.
- A configured unit has no unit id. The plan says what to build, not what to call it.
- `depends` are the units of the libraries the unit builds against. A dependency on a sublibrary is the id of a unit whose `component` is `lib:<name>`.
- `exe-depends` are the units of the executables it needs at build time (`build-tool-depends`, and known `build-tools`).
- `setup-depends` are the libraries the package's `Setup.hs` needs. Only whole-package units have any.

`build-style` says how the unit is built, following cabal-install 3.14:

- **`per-component`**: the unit is one component, named in `component` (`lib`, `lib:<name>`, `flib:<name>`, `exe:<name>`, `test:<name>` or `bench:<name>`). Packages are built this way unless one of the conditions below applies.
- **`whole-package`**: the unit is the whole package, built with its `Setup.hs`. `components` lists the components it builds, and `depends` and `exe-depends` are the union over them, without dependencies between the package's own components. This applies when the build-type is `Configure`, `Custom` or `Hooks`, when the `cabal-version` is older than 1.8, or when the package has no buildable components. (cabal-install 3.18 builds `Configure` packages per component; this tool does not.)

A target package gets all of its buildable libraries, foreign libraries and executables. Other packages only get the components something in the plan needs. Test suites and benchmarks are only included when the solver enabled them.

This is a difference from cabal-install's `plan.json`, which is written before pruning to the targets: it also lists library components that nothing needs. One example is the internal library `vector:lib:benchmarks-O2`, which only vector's benchmarks use, together with its dependencies such as `tasty`.

### Backpack

Backpack is not supported. An indefinite library (one with `signatures`) has to be instantiated, which gives extra units that this tool does not make. If a unit in the plan is such a library, or is an indefinite library from a package db, `cabal-solver-plan --json` prints nothing on stdout, names the units on stderr and exits with status 1, as when no plan is found. Libraries that only rename modules with `mixins` are fine.

### Packages from `--with-preinstalled`

A package given with `--with-preinstalled` is not in any package db, so a plan cannot say where to find it. If the solver chooses one, `cabal-solver-plan --json` prints nothing on stdout, names the packages on stderr and exits with status 1. Use `--package-db` instead.
