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
trap 'rm -rf "$TMPDIR"' EXIT

if git rev-parse --verify "$BRANCH" >/dev/null 2>&1; then
  git worktree add --quiet "$TMPDIR" "$BRANCH"
else
  echo "Error: branch '$BRANCH' does not exist." >&2
  exit 1
fi

mkdir -p "$TMPDIR/$DATA_DIR"

if [ -f "$TMPDIR/$DATA_FILE" ]; then
  # Append the new entry to the existing Benchmark array.
  # The file ends with: ... } \n    ] \n  } \n}
  # We need to insert ",\n<new_entry>" before the final closing of the array.
  #
  # Strategy: find the last "]" that closes the Benchmark array and insert before it.
  Rscript --vanilla -e '
    args <- commandArgs(TRUE)
    lines <- readLines(args[1], warn = FALSE)
    text <- paste(lines, collapse = "\n")

    # Strip JS prefix
    json_text <- sub("^window\\.BENCHMARK_DATA = ", "", text)
    new_entry <- paste(readLines(args[2], warn = FALSE), collapse = "\n")

    # Find the last occurrence of the closing bracket for the Benchmark array.
    # The structure is: ... "Benchmark": [ {entry}, {entry} ] } }
    # We insert before the final "]\n  }\n}" sequence.
    # Use a simple approach: find the last "]" that is followed only by
    # closing braces and whitespace.
    pattern <- "\\]\\s*\\}\\s*\\}\\s*$"
    m <- regexpr(pattern, json_text)
    if (m == -1) stop("Could not find end of Benchmark array in data.js")

    before <- substr(json_text, 1, m - 1)
    after <- substr(json_text, m, nchar(json_text))

    updated <- paste0(before, ",\n", new_entry, "\n", after)

    # Update lastUpdate timestamp
    updated <- sub(
      "\"lastUpdate\": *[0-9.]+",
      sprintf("\"lastUpdate\": %.2f", as.numeric(Sys.time()) * 1000),
      updated
    )

    writeLines(paste0("window.BENCHMARK_DATA = ", updated), args[3])
  ' "$TMPDIR/$DATA_FILE" <(echo "$NEW_ENTRY") "$TMPDIR/$DATA_FILE"
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

git worktree remove --force "$TMPDIR" 2>/dev/null || true
trap - EXIT

echo "Benchmark results committed to $BRANCH branch."
echo "Run 'git push origin $BRANCH' to publish."
