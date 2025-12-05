#!/usr/bin/env bash
set -euo pipefail

VERSION_FILE="VERSION.md"
PR_TITLE="$1"

CURRENT_VERSION=$(cat "$VERSION_FILE" | sed 's/^v//')
IFS='.' read -r MAJOR MINOR PATCH <<< "$CURRENT_VERSION"

MAJOR=${MAJOR:-0}
MINOR=${MINOR:-0}
PATCH=${PATCH:-0}

PR_TYPE=$(echo "$PR_TITLE" | cut -d':' -f1 | tr '[:upper:]' '[:lower:]')

declare -A TYPE_MAP=(
    ["fix"]="PATCH"
    ["perf"]="PATCH"
    ["refactor"]="PATCH"
    ["feat"]="MINOR"
    ["breaking"]="MAJOR"
    ["docs"]="NONE"
    ["style"]="NONE"
    ["chore"]="NONE"
    ["test"]="NONE"
    ["build"]="NONE"
    ["ci"]="NONE"
)

BUMP_TYPE="${TYPE_MAP[$PR_TYPE]:-PATCH}"

if [[ "$BUMP_TYPE" == "NONE" ]]; then
    echo ""
    exit 0
fi

case "$BUMP_TYPE" in
    MAJOR)
        MAJOR=$((MAJOR + 1))
        MINOR=0
        PATCH=0
        ;;
    MINOR)
        MINOR=$((MINOR + 1))
        PATCH=0
        ;;
    PATCH)
        PATCH=$((PATCH + 1))
        ;;
esac

NEW_VERSION="$MAJOR.$MINOR.$PATCH"

echo "$NEW_VERSION"
