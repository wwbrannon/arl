#!/usr/bin/env bash
# Publish benchmark results to gh-pages branch.
# Mirrors what github-action-benchmark does: reads benchmark-results.json,
# wraps it in a new entry with commit metadata, and appends it to
# dev/bench/data.js on the gh-pages branch.
#
# Usage: benchmarks/publish-results.sh
# Prereq: benchmarks/results/benchmark-results.json must exist (run `make bench`)

set -euo pipefail

RESULTS_FILE="benchmarks/results/benchmark-results.json"
DATA_DIR="dev/bench"
DATA_FILE="$DATA_DIR/data.js"
BRANCH="gh-pages"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [ ! -f "$RESULTS_FILE" ]; then
  echo "Error: $RESULTS_FILE not found. Run 'make bench' first." >&2
  exit 1
fi

# Gather commit metadata from HEAD
COMMIT_ID=$(git rev-parse --short HEAD)
COMMIT_MSG=$(git log -1 --format='%s' | sed 's/"/\\"/g')
COMMIT_TIME=$(git log -1 --format='%aI')
COMMIT_URL="$(git remote get-url origin | sed 's/\.git$//' | sed 's|git@github.com:|https://github.com/|')/commit/$COMMIT_ID"
AUTHOR=$(git log -1 --format='%an')
DATE_MS=$(date +%s)000

# Build the new entry JSON
NEW_ENTRY=$(cat <<EOF
      {
        "commit": {
          "author": {"username": "$AUTHOR"},
          "committer": {"username": "$AUTHOR"},
          "id": "$COMMIT_ID",
          "message": "$COMMIT_MSG",
          "timestamp": "$COMMIT_TIME",
          "url": "$COMMIT_URL"
        },
        "date": $DATE_MS,
        "tool": "customSmallerIsBetter",
        "benches": $(cat "$RESULTS_FILE")
      }
EOF
)

# Work in a temporary worktree so we don't disturb the current checkout
TMPDIR=$(mktemp -d)
WORKTREE_ADDED=false

cleanup() {
  if [ "$WORKTREE_ADDED" = true ]; then
    git worktree remove --force "$TMPDIR" 2>/dev/null || true
  fi
  rm -rf "$TMPDIR"
}
trap cleanup EXIT

if git rev-parse --verify "$BRANCH" >/dev/null 2>&1; then
  git worktree add --quiet "$TMPDIR" "$BRANCH"
  WORKTREE_ADDED=true
else
  echo "Error: branch '$BRANCH' does not exist." >&2
  exit 1
fi

mkdir -p "$TMPDIR/$DATA_DIR"

if [ -f "$TMPDIR/$DATA_FILE" ]; then
  # Append the new entry to the existing Benchmark array.
  # Write new entry to a temp file so R doesn't have to read from a pipe.
  ENTRY_FILE="$TMPDIR/.new-entry.json"
  echo "$NEW_ENTRY" > "$ENTRY_FILE"

  Rscript --vanilla "$SCRIPT_DIR/append-bench-entry.R" \
    "$TMPDIR/$DATA_FILE" "$ENTRY_FILE" "$TMPDIR/$DATA_FILE"

  rm -f "$ENTRY_FILE"
else
  # First entry — create the file from scratch
  REPO_URL=$(git remote get-url origin | sed 's/\.git$//' | sed 's|git@github.com:|https://github.com/|')
  cat > "$TMPDIR/$DATA_FILE" <<DATAEOF
window.BENCHMARK_DATA = {
  "lastUpdate": ${DATE_MS},
  "repoUrl": "${REPO_URL}",
  "entries": {
    "Benchmark": [
${NEW_ENTRY}
    ]
  }
}
DATAEOF
fi

# Commit and clean up the worktree
cd "$TMPDIR"
git add "$DATA_FILE"
git commit -m "Add benchmark results for $COMMIT_ID"
cd - >/dev/null

echo "Benchmark results committed to $BRANCH branch."
echo "Run 'git push origin $BRANCH' to publish."
