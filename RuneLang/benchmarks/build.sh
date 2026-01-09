#!/bin/bash

GREEN='\033[0;32m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${CYAN}=======================================${NC}"
echo -e "${CYAN}          BENCHMARK TESTER${NC}"
echo -e "${CYAN}=======================================${NC}"

BASE_DIR="$(dirname "$(realpath "$0")")"
RUNE_EXEC="$(realpath $PWD/rune)"

function run_bench()
{
    local lang=$1
    local cmd=$2
    echo -ne "${YELLOW}[$lang]${NC} Running... "

    local start=$(date +%s.%N)
    $cmd > /dev/null 2>&1
    local status=$?
    local end=$(date +%s.%N)

    if [ $status -eq 0 ]; then
        local time_res=$(echo "$end - $start" | bc)
        echo -e "${GREEN}Done in ${time_res}s${NC}"
    else
        echo -e "${RED}Failed${NC}"
    fi
}

find "$BASE_DIR" -mindepth 1 -type d | while read -r subdir; do
    echo -e "\n${CYAN}Module: $(basename "$subdir")${NC}"
    cd "$subdir" || continue

    for file in *; do
        filename=$(basename -- "$file")
        extension="${filename##*.}"
        name="${filename%.*}"

        case "$extension" in
            go)
                go build -o "${name}" "$filename" && \
                run_bench "Go" "./${name}"
                rm -f "${name}"
                ;;
            c)
                gcc -O3 -o "${name}" "$filename" && \
                run_bench "C" "./${name}"
                rm -f "${name}"
                ;;
            java)
                javac "$filename" && \
                run_bench "Java" "java $name"
                rm -f *.class
                rm -f "${name}"
                ;;
            cr)
                crystal build --release "$filename" -o "${name}" && \
                run_bench "Crystal" "./${name}"
                rm -f "${name}"
                ;;
            ru)
                $RUNE_EXEC build "$filename" -o "${name}"
                run_bench "Rune" "./${name}"
                rm -f "${name}"
                ;;
            *)
                ;;
        esac
    done
done

echo -e "\n${CYAN}=======================================${NC}"
echo -e "${CYAN}        BENCHMARK COMPLET             ${NC}"
echo -e "${CYAN}=======================================${NC}"

find "$BASE_DIR" -type f -name "*.o" -exec rm -f {} \;
