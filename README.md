# cabal-solver-plan

Run cabal-install solver from inputs specified on the command line (most of them).

```
❯ cabal run -- cabal-solver-plan
Missing: (--with-ghc[=GHC] | --compiler-id ARG)

Usage: cabal-solver-plan (--with-ghc[=GHC] [--package-db GLOBAL|USER|PATH] |
                           --compiler-id ARG [--abi-tag ARG]
                           [(--compiler-id-compat ARG)] [(--language ARG)]
                           [(--extension ARG)] [--os OS] [--arch ARCH]
                           [--package-db PATH]) [--repository ARG]
                         [--index-state ARG]
                         [--pkgconfig-entry ARG | --use-system-pkgconfig]
                         [--constraint ARG] [--flag ARG] [--reorder-goals]
                         [--count-conflicts] [--fine-grained-conflicts]
                         [--minimize-conflict-set] [--independent-goals]
                         [--avoid-reinstalls] [--shadow-pkgs] [--strong-flags]
                         [--allow-boot-lib-installs] [--only-constrainted ARG]
                         [--max-backjumps ARG] [--enable-backjumping]
                         [--solve-executables] [--verbosity ARG]
                         [--prune-after-first-success] [--prefer-oldest]
                         [--cache-dir ARG] [--offline] [TARGET]
```

## Examples

Use the system ghc:

```
❯ cabal run -- cabal-solver-plan --with-ghc
[__0] done
```

You need to specify one or more targets. E.g. let's find a plan for `mtl`.

```
❯ cabal run -- cabal-solver-plan --with-ghc mtl
[__0] unknown package: mtl (user goal)
[__0] fail (backjumping, conflict set: mtl)
Exhaustive search failed
conflict set: mtl
conflict map: fromList [(P (Q (PackagePath DefaultNamespace QualToplevel) (PackageName "mtl")),1)]
```

Of course, what's `mtl` after all?

```
❯ cabal run -- cabal-solver-plan --with-ghc --package-db global mtl
[__0] trying: mtl-2.2.2/installed-2.2.2 (user goal)
[__1] trying: base-4.17.1.0/installed-4.17.1.0 (dependency of mtl)
[__2] trying: rts-1.0.2/installed-1.0.2 (dependency of base)
[__3] trying: ghc-prim-0.9.0/installed-0.9.0 (dependency of base)
[__4] trying: ghc-bignum-1.3/installed-1.3 (dependency of base)
[__5] next goal: transformers (dependency of mtl)
[__5] trying: transformers-0.5.6.2/installed-0.5.6.2
[__6] done
rts-1.0.2/installed-1.0.2

ghc-prim-0.9.0/installed-0.9.0
lib     rts-1.0.2/installed-1.0.2

ghc-bignum-1.3/installed-1.3
lib     ghc-prim-0.9.0/installed-0.9.0

base-4.17.1.0/installed-4.17.1.0
lib     ghc-bignum-1.3/installed-1.3, ghc-prim-0.9.0/installed-0.9.0, rts-1.0.2/installed-1.0.2

transformers-0.5.6.2/installed-0.5.6.2
lib     base-4.17.1.0/installed-4.17.1.0

mtl-2.2.2/installed-2.2.2
lib     base-4.17.1.0/installed-4.17.1.0, transformers-0.5.6.2/installed-0.5.6.2
```

Something from hackage

```
 cabal-solver-plan --with-ghc --package-db global --repository https://hackage.haskell.org aeson
Loading repository1:https://hackage.haskell.org, checking for updates ... updates available, refreshing the cache ...
 done
Active repositories: repository1:merge
Total index-state: repository1 2023-08-04T02:18:50Z
[__0] trying: aeson-2.2.0.0 (user goal)
...
[140] trying: aeson:+ordered-keymap[141] done
...
aeson-2.2.0.0 +ordered-keymap
lib     OneTuple-0.4.1.1, QuickCheck-2.14.3, base-4.17.1.0/installed-4.17.1.0, bytestring-0.11.4.0/installed-0.11.4.0, containers-0.6.7/installed-0.6.7, data-fix-0.3.2, deepseq-1.4.8.0/installed-1.4.8.0, dlist-1.0, exceptions-0.10.5/installed-0.10.5, generically-0.1.1, ghc-prim-0.9.0/installed-0.9.0, hashable-1.4.3.0, indexed-traversable-0.1.2.1, integer-conversion-0.1, network-uri-2.6.4.2, primitive-0.8.0.0, scientific-0.3.7.0, semialign-1.3, strict-0.5, tagged-0.8.7, template-haskell-2.19.0.0/installed-2.19.0.0, text-2.0.2/installed-2.0.2, text-iso8601-0.1, text-short-0.1.5, th-abstraction-0.5.0.0, these-1.2, time-1.12.2/installed-1.12.2, time-compat-1.9.6.1, unordered-containers-0.2.19.1, uuid-types-1.0.5, vector-0.13.0.0, witherable-0.4.2
```
