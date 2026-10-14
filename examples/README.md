# Example plans

JSON plans (see `../docs/plan-format.md`) for the targets `network` and `shake`. They were made with GHC 9.12.2 (the bindist timbuktu's buck2 toolchain uses) at index-state 2026-06-01T00:00:00Z:

```sh
cabal-solver-plan -w <ghc-9.12.2> \
  --index <copy of>/01-index.tar --index-state 2026-06-01T00:00:00Z \
  --json network > network.json
```

They match cabal-install 3.14.2's plans for the same targets, which are in `../timbuktu/examples/plan/{network,shake}/BUCK`.
