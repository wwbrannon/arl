#!/bin/sh
# Run R CMD check with strict env vars from check.env.
# Usage: tools/build/check-local.sh <NOT_CRAN>
# where NOT_CRAN is "true" (run all tests) or "false" (CRAN thinning).

set -e

not_cran="${1:?Usage: check-local.sh <true|false>}"

pkg=$(Rscript -e 'p <- read.dcf("DESCRIPTION"); cat(p[1,"Package"])')
tarball=$(Rscript -e 'p <- read.dcf("DESCRIPTION"); cat(sprintf("%s_%s.tar.gz", p[1,"Package"], p[1,"Version"]))')

start=$(date +%s)
check_tmpdir=$(mktemp -d)

win_overrides=
if Rscript -e 'cat(.Platform$OS.type)' | grep -q windows; then
  win_overrides="_R_CHECK_CRAN_INCOMING_USE_ASPELL_=false"
fi

# shellcheck disable=SC2046
TMPDIR="$check_tmpdir" NOT_CRAN="$not_cran" \
  env $(grep -v '^\#' tools/check.env | xargs) $win_overrides \
  R CMD check --as-cran --run-donttest "$tarball"
rc=$?

elapsed=$(( $(date +%s) - start ))
printf '\nR CMD check completed in %dm %ds\n' $((elapsed/60)) $((elapsed%60))
rm -rf "$check_tmpdir"

if [ $rc -ne 0 ]; then exit $rc; fi
if ! grep -q '^Status: OK' "$pkg.Rcheck/00check.log"; then
  echo "R CMD check finished with warnings or notes:"
  grep '^Status:' "$pkg.Rcheck/00check.log"
  exit 1
fi
