#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#gnused --command bash
# shellcheck shell=bash
# Benchmarks against baseline.csv: fails if anything is >10% slower, or if a
# baseline benchmark is gone. tasty-bench passes a renamed benchmark
# vacuously, so the missing-name check is ours.
set -euo pipefail
cd "$(dirname "$0")/.."

[ -f baseline.csv ] || {
  echo "compare: no baseline.csv at the repo root" >&2
  exit 2
}

bench=0
nix develop -c cabal bench --benchmark-options='--csv current.csv --baseline baseline.csv --fail-if-slower 10' || bench=$?

indent() { sed 's/^/  /'; }
# Names are everything before the last two (numeric) fields.
names() { tail -n +2 "$1" | sed -E 's/,[^,]*,[^,]*$//' | sort -u; }

missing=$(comm -23 <(names baseline.csv) <(names current.csv))
added=$(comm -13 <(names baseline.csv) <(names current.csv))

if [ -n "$added" ]; then
  echo "informational: new benchmarks, not in baseline.csv:"
  indent <<<"$added"
fi
if [ -n "$missing" ]; then
  echo "compare: FAIL, baseline benchmarks missing from current.csv:" >&2
  indent <<<"$missing" >&2
  exit 1
fi
if [ "$bench" -ne 0 ]; then
  echo "compare: FAIL, cabal bench exited $bench (slower than baseline by >10%?)" >&2
  exit "$bench"
fi
echo "compare: ok"
