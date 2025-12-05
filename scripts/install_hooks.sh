#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_PATH="$(cd "$SCRIPT_DIR/.." && pwd)"
HOOK_SRC="$REPO_PATH/scripts/.githooks/pre_push"
HOOK_DEST="$REPO_PATH/.git/hooks/pre-push"

if [ ! -f "$HOOK_SRC" ]; then
    echo -e "\033[1;31mERROR:\033[0m $HOOK_SRC file not found!"
    exit 1
fi

if [ ! -d "$REPO_PATH/.git" ]; then
    echo -e "\033[1;31mERROR:\033[0m This is not a git repository!"
    exit 1
fi

mkdir -p "$REPO_PATH/.git/hooks"

cp "$HOOK_SRC" "$HOOK_DEST"
if [ $? -ne 0 ]; then
    echo -e "\033[1;31mERROR:\033[0m Failed to copy the pre-push hook to $HOOK_DEST!"
    exit 1
fi

chmod +x "$HOOK_DEST"
if [ $? -ne 0 ]; then
    echo -e "\033[1;31mERROR:\033[0m Failed to make the pre-push hook executable!"
    exit 1
fi

echo -e "\033[1;32mGit hooks installed successfully at $HOOK_DEST\033[0m"
