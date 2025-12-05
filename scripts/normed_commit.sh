#!/bin/bash

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <git add args> (ex: -A, ., file1 file2 ...)"
    exit 1
fi

if ! git add "$@"; then
    echo "Failed to add files"
    exit 1
fi

declare -A TYPES_DESC=(
    # PATCH
    ["fix"]="Bug fix"
    ["perf"]="Performance improvement"
    ["refactor"]="Code refactoring without functional change"
    # MINOR
    ["feat"]="New feature"
    # MAJOR (should be accompanied by BREAKING CHANGE in the message)
    ["BREAKING"]="Major incompatible change"
    # No SemVer impact
    ["docs"]="Documentation changes"
    ["style"]="Code style / formatting changes"
    ["chore"]="Maintenance tasks"
    ["test"]="Add or fix tests"
    ["build"]="Build system changes"
    ["ci"]="CI/CD changes"
)

# Color codes (variants)
PATCH_COLOR='\033[1;32m'   # light green
MINOR_COLOR='\033[1;33m'   # light yellow
MAJOR_COLOR='\033[1;31m'   # light red
OTHERS_COLOR='\033[0;37m'  # gray
NC='\033[0m'               # No Color

echo -e "Possible commit types:"
echo -e "${PATCH_COLOR}PATCH:${NC}"
for t in fix perf refactor; do
    echo -e "  $t : ${TYPES_DESC[$t]}"
done

echo -e "${MINOR_COLOR}MINOR:${NC}"
for t in feat; do
    echo -e "  $t : ${TYPES_DESC[$t]}"
done

echo -e "${MAJOR_COLOR}MAJOR:${NC}"
for t in BREAKING; do
    echo -e "  $t : ${TYPES_DESC[$t]}"
done

echo -e "${OTHERS_COLOR}OTHERS (do not affect SemVer):${NC}"
for t in docs style chore test build ci; do
    echo -e "  $t : ${TYPES_DESC[$t]}"
done
echo ""

while true; do
    read -p "Enter the commit type: " TYPE
    if [[ -n "${TYPES_DESC[$TYPE]}" ]]; then
        break
    else
        echo -e "\033[1;31mInvalid type, please choose a valid type.\033[0m"
    fi
done

read -p "Enter the scope (optional, press Enter for none): " SCOPE
if [ -n "$SCOPE" ]; then
    SCOPE="($SCOPE)"
fi

read -p "Enter the commit message: " MESSAGE

COMMIT_MSG="$TYPE$SCOPE: $MESSAGE"

if ! git commit -m "$COMMIT_MSG"; then
    echo "Commit failed or was cancelled"
    exit 1
fi

read -p "Do you want to push the commit to the remote repository now? [y/N]: " PUSH_CONFIRM
if [[ "$PUSH_CONFIRM" =~ ^[Yy]$ ]]; then
    git push
    echo "Commit and push done: $COMMIT_MSG"
else
    echo "Commit done (not pushed): $COMMIT_MSG"
fi
