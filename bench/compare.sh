#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#gnused nixpkgs#gawk nixpkgs#util-linux --command bash
# shellcheck shell=bash
# Fails if a benchmark is >10% slower or gone (tasty-bench passes a renamed
# benchmark vacuously, so the missing-name check is ours).
#
#   bench/compare.sh            HEAD against the committed baseline.csv
#   bench/compare.sh BASE_REV   A/B: builds BASE_REV and HEAD first, then
#                               measures both on the same cores (CORES,
#                               default 2,3), interleaved A B A B; the
#                               second pair decides, the first shows noise.
set -euo pipefail
cd "$(dirname "$0")/.."

fail_if_slower=10
indent() { sed 's/^/  /'; }
# Names are everything before the last two (numeric) fields.
names() { tail -n +2 "$1" | sed -E 's/,[^,]*,[^,]*$//' | sort -u; }

# Exits 1 if a name in $1 is missing from $2; lists names only in $2.
check_names() {
  local missing added
  missing=$(comm -23 <(names "$1") <(names "$2"))
  added=$(comm -13 <(names "$1") <(names "$2"))
  if [ -n "$added" ]; then
    echo "informational: new benchmarks, not in the baseline:"
    indent <<<"$added"
  fi
  if [ -n "$missing" ]; then
    echo "compare: FAIL, baseline benchmarks missing:" >&2
    indent <<<"$missing" >&2
    exit 1
  fi
}

# The largest relative difference between two runs of the same binary.
spread() {
  awk -F, 'NR == FNR { if (FNR > 1) m[$1] = $(NF - 1); next }
           FNR > 1 && ($1 in m) && m[$1] > 0 {
             d = ($(NF - 1) - m[$1]) / m[$1]; if (d < 0) d = -d
             if (d > best) { best = d; at = $1 }
           }
           END { printf "max %.0f%% (%s)\n", best * 100, at }' "$1" "$2"
}

if [ $# -eq 0 ]; then
  [ -f baseline.csv ] || {
    echo "compare: no baseline.csv at the repo root" >&2
    exit 2
  }
  bench=0
  nix develop -c cabal bench --benchmark-options="--csv current.csv --baseline baseline.csv --fail-if-slower $fail_if_slower" || bench=$?
  check_names baseline.csv current.csv
  if [ "$bench" -ne 0 ]; then
    echo "compare: FAIL, cabal bench exited $bench (slower than baseline by >${fail_if_slower}%?)" >&2
    exit "$bench"
  fi
  echo "compare: ok"
  exit 0
fi

base_rev=$1
cores=${CORES:-2,3}
tmp=$(mktemp -d)
cleanup() {
  git worktree remove --force "$tmp/base" 2>/dev/null || true
  rm -r "${tmp:?}"
}
trap cleanup EXIT

# Build everything before measuring anything, so no build overlaps a run.
git worktree add --detach --quiet "$tmp/base" "$base_rev"
build() { (cd "$1" && nix develop -c cabal build -v0 bench && nix develop -c cabal list-bin bench | tail -n 1); }
base_bin=$(build "$tmp/base")
head_bin=$(build .)

read -r load _ </proc/loadavg
if awk -v l="$load" 'BEGIN { exit !(l > 2) }'; then
  echo "compare: WARNING, load average $load > 2; numbers will be noisy" >&2
fi

# run BIN --csv FILE [ARGS]: the log lands next to FILE.
run() { taskset -c "$cores" "$@" >"${3%.csv}.log" 2>&1; }
echo "measuring $(git rev-parse --short "$base_rev") (A) against HEAD $(git rev-parse --short HEAD) (B) on cores $cores"
run "$base_bin" --csv "$tmp/a1.csv"
run "$head_bin" --csv "$tmp/b1.csv"
run "$base_bin" --csv "$tmp/a2.csv"
bench=0
run "$head_bin" --csv "$tmp/b2.csv" --baseline "$tmp/a2.csv" --fail-if-slower "$fail_if_slower" || bench=$?

echo "noise, A against itself: $(spread "$tmp/a1.csv" "$tmp/a2.csv")"
echo "noise, B against itself: $(spread "$tmp/b1.csv" "$tmp/b2.csv")"
check_names "$tmp/a2.csv" "$tmp/b2.csv"
if [ "$bench" -ne 0 ]; then
  grep -B 1 'FAIL' "$tmp/b2.log" | grep -v '^--$' | indent >&2 || true
  echo "compare: FAIL, HEAD slower than $base_rev by >${fail_if_slower}% (see the noise lines above)" >&2
  exit "$bench"
fi
echo "compare: ok"
