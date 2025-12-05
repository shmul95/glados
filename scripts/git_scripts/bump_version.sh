#!/usr/bin/env bash
set -e

VERSION_FILE="VERSION.md"
PR_TITLE="$1"
BRANCH="$2"

VERSION=$(cat $VERSION_FILE | sed 's/^v//')
IFS='.' read -r MAJOR MINOR PATCH <<< "${VERSION%%-*}"

NEW_VERSION="$VERSION"

if [[ "$BRANCH" == "dev" ]]; then
    if [[ $PR_TITLE =~ ^feat ]]; then
        ((MINOR++))
        PATCH=0
    elif [[ $PR_TITLE =~ ^fix ]]; then
        ((PATCH++))
    elif [[ $PR_TITLE =~ ^BREAKING ]]; then
        ((MAJOR++))
        MINOR=0
        PATCH=0
    else
        echo "PR title does not match conventional bump. Using patch as default."
        ((PATCH++))
    fi

    LAST_BETA=$(git tag --list "v$MAJOR.$MINOR.$PATCH-beta.*" | sort -V | tail -n1)
    if [[ -z "$LAST_BETA" ]]; then
        BETA=1
    else
        BETA=$(( ${LAST_BETA##*-beta.} + 1 ))
    fi

    NEW_VERSION="$MAJOR.$MINOR.$PATCH-beta.$BETA"

else
    NEW_VERSION="$MAJOR.$MINOR.$PATCH"
fi

echo "New version: $NEW_VERSION"
echo "$NEW_VERSION" > $VERSION_FILE

git config user.name "github-actions[bot]"
git config user.email "github-actions[bot]@users.noreply.github.com"
git add $VERSION_FILE
git commit -m "chore(release): bump version to $NEW_VERSION"
git tag "v$NEW_VERSION"
git push origin HEAD --tags
