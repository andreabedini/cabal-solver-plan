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

# fail NAME [FILE]: report a failure, with the end of FILE if given. On
# GitHub Actions it is also an annotation, which, unlike the log, anyone
# can read through the API.
fail() {
    printf 'FAIL %s\n' "$1"
    failures=$((failures + 1))
    if [ -n "${2:-}" ]; then
        tail -n 50 "$2"
    fi
    if [ "${GITHUB_ACTIONS:-}" = true ]; then
        details=$(if [ -n "${2:-}" ]; then tail -n 20 "$2"; fi | tr -d '\r' | sed 's/%/%25/g' | awk '{ printf "%s%%0A", $0 }')
        # In the title, a property value, "," and ":" have to be escaped too.
        title=$(printf 'tests/run.sh %s' "$1" | sed 's/%/%25/g; s/,/%2C/g; s/:/%3A/g')
        printf '::error title=%s::%s\n' "$title" "$details"
    fi
}

# check NAME CMD...: run CMD, with stdout in $tmp/NAME.out; show its stderr if it fails.
check() {
    name=$1
    shift
    if "$@" >"$tmp/$name.out" 2>"$tmp/$name.err"; then
        pass "$name"
    else
        fail "$name" "$tmp/$name.err"
        return 1
    fi
}

# check_exit NAME CODE CMD...: check that CMD exits with CODE.
check_exit() {
    name=$1
    code=$2
    shift 2
    actual=0
    "$@" >"$tmp/$name.out" 2>"$tmp/$name.err" || actual=$?
    if [ "$actual" -eq "$code" ]; then
        pass "$name"
    else
        fail "$name (exit $actual, expected $code)" "$tmp/$name.err"
    fi
}

version=$(sed -n 's/^version: *//p' cabal-solver-plan.cabal)
if check version "$bin" --version; then
    if [ "$(tr -d '\r' <"$tmp/version.out")" != "cabal-solver-plan $version" ]; then
        fail "version is $version" "$tmp/version.out"
    fi
fi

check help "$bin" --help || true

# Usage errors exit with 2.
check_exit no-arguments 2 "$bin"
check_exit unknown-option 2 "$bin" --no-such-option mtl
check_exit conflicting-options 2 "$bin" --compiler-id ghc-9.12.2 -w "$ghc" mtl
check_exit compiler-id-use-global-packagedb 2 "$bin" --compiler-id ghc-9.12.2 --use-global-packagedb mtl
check_exit contradicting-global-package-db 2 "$bin" --use-global-packagedb --no-global-package-db mtl

# No plan exits with 1, and other errors with 3, without an exception.
check_exit no-plan 1 "$bin" -w "$ghc" no-such-package
check_exit missing-package-db 3 "$bin" -w "$ghc" --package-db tests/no-such-db mtl
if grep -q -i exception "$tmp/missing-package-db.err"; then
    fail "missing-package-db prints no exception" "$tmp/missing-package-db.err"
fi

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
        diff -u "$golden" "$tmp/$target.json" >"$tmp/$target.diff" || true
        fail "golden-$target" "$tmp/$target.diff"
    else
        pass "golden-$target"
    fi
done

if [ "$failures" -ne 0 ]; then
    printf '%d failure(s)\n' "$failures"
    exit 1
fi
