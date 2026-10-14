#!/bin/sh
# Test a built cabal-solver-plan executable:
#
#   tests/run.sh PATH/TO/cabal-solver-plan
#
# The golden tests download Hackage's index into $CACHE_DIR (default _cache),
# or update it, and solve for ghc-9.12.2 on x86_64-linux with the package db in
# tests/package-db, so they give the same plans on every platform. The smoke
# tests need a GHC ($GHC, default ghc).
#
# Set UPDATE_GOLDEN=1 to rewrite tests/golden instead of comparing with it.
set -eu

bin=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
cd "$(dirname "$0")/.."

ghc=${GHC:-ghc}
cache_dir=${CACHE_DIR:-_cache}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
failures=0

pass() { printf 'PASS %s\n' "$1"; }
fail() {
    printf 'FAIL %s\n' "$1"
    failures=$((failures + 1))
}

# check NAME CMD...: run CMD, with stdout in $tmp/NAME.out; show its stderr if it fails.
check() {
    name=$1
    shift
    if "$@" >"$tmp/$name.out" 2>"$tmp/$name.err"; then
        pass "$name"
    else
        fail "$name"
        tail -n 50 "$tmp/$name.err"
        return 1
    fi
}

version=$(sed -n 's/^version: *//p' cabal-solver-plan.cabal)
if check version "$bin" --version; then
    if [ "$(tr -d '\r' <"$tmp/version.out")" != "cabal-solver-plan $version" ]; then
        fail "version is $version"
        cat "$tmp/version.out"
    fi
fi

check help "$bin" --help || true

check with-compiler "$bin" -w "$ghc" mtl || true

# The old names of -w and of the default --use-global-packagedb still work.
check deprecated-options "$bin" --with-ghc "$ghc" --use-global-packagedb mtl || true

for target in network shake; do
    check "solve-$target" "$bin" \
        --compiler-id ghc-9.12.2 --platform x86_64-linux \
        --package-db tests/package-db/ghc-9.12.2-x86_64-linux \
        --repository hackage.haskell.org:https://hackage.haskell.org/ \
        --cache-dir "$cache_dir" \
        --index-state 2026-06-01T00:00:00Z \
        --json "$target" || continue
    golden=tests/golden/$target.json
    tr -d '\r' <"$tmp/solve-$target.out" >"$tmp/$target.json"
    if [ "${UPDATE_GOLDEN:-}" = 1 ]; then
        cp "$tmp/$target.json" "$golden"
    elif ! cmp -s "$golden" "$tmp/$target.json"; then
        fail "golden-$target"
        diff -u "$golden" "$tmp/$target.json" | head -n 100 || true
    else
        pass "golden-$target"
    fi
done

if [ "$failures" -ne 0 ]; then
    printf '%d failure(s)\n' "$failures"
    exit 1
fi
