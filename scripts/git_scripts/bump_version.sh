#!/usr/bin/env bash
set -euo pipefail

VERSION_FILE="VERSION.md"
PR_TITLE="$1"
BRANCH="$2"

VERSION=$(cat "$VERSION_FILE" | sed 's/^v//')
IFS='.' read -r MAJOR MINOR PATCH <<< "${VERSION%%-*}"

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
    exit 0
fi

if [[ "$BRANCH" == "dev" ]]; then
    [[ $BUMP_TYPE == "MAJOR" ]] && ((MAJOR++)) && MINOR=0 && PATCH=0
    [[ $BUMP_TYPE == "MINOR" ]] && ((MINOR++)) && PATCH=0
    [[ $BUMP_TYPE == "PATCH" ]] && ((PATCH++))

    LAST_BETA=$(git tag --list "v$MAJOR.$MINOR.$PATCH-beta.*" | sort -V | tail -n1)
    BETA=$(( ${LAST_BETA##*-beta.} + 1 ))
    [[ "$LAST_BETA" == "" ]] && BETA=1

    NEW_VERSION="$MAJOR.$MINOR.$PATCH-beta.$BETA"
else
    [[ $BUMP_TYPE == "MAJOR" ]] && ((MAJOR++)) && MINOR=0 && PATCH=0
    [[ $BUMP_TYPE == "MINOR" ]] && ((MINOR++)) && PATCH=0
    [[ $BUMP_TYPE == "PATCH" ]] && ((PATCH++))

    NEW_VERSION="$MAJOR.$MINOR.$PATCH"
fi

echo "New version: $NEW_VERSION"

git config user.name "github-actions[bot]"
git config user.email "github-actions[bot]@users.noreply.github.com"
git add "$VERSION_FILE"
git commit -m "chore(release): bump version to $NEW_VERSION"
git tag "v$NEW_VERSION"
git push origin HEAD --tags

echo "$NEW_VERSION"
