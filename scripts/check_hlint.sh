#!/usr/bin/env bash

RED='\033[1;31m'
GREEN='\033[1;32m'
NC='\033[0m'

SUCCESS=0
ERROR=84

TARGET_DIR="$PWD/RuneLang/src/"

if ! command -v hlint &> /dev/null; then
    echo -e "${RED}Error: hlint is not installed.${NC}"
    exit $ERROR
fi

hlint $TARGET_DIR
RET=$?

if [ $RET -ne 0 ]; then
    echo -e "\n${RED}[!] HLint found suggestion(s).${NC}"
    exit $ERROR
else
    echo -e "\n${GREEN}[✓] No hints found.${NC}"
    exit $SUCCESS
fi

