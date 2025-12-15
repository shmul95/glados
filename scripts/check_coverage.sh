#!/usr/bin/env bash

GREEN="\033[1;32m"
RED="\033[1;31m"
ILC="\033[3m"
ORG="\033[1;33m"
RST="\033[0m"
BOLD="\033[1m"

MINIMUM_COVERAGE=85
EXIT_SUCCESS=0
EXIT_FAILURE=84

EXIT_CODE=0

LOG_CONTENT=$(cat /dev/stdin)

set -e

function _error()
{
    echo -e "${RED}${BOLD}[❌] ERROR:\n${RST}\t$1\n\t${ILC}\"$2\"${RST}"
}

function _success()
{
    echo -e "${GREEN}[✅] SUCCESS:\t${RST} ${ILC}$1${RST}"
}

function _info()
{
    echo -e "${ORG}[🚧] RUNNING:\t${RST} ${ILC}$1${RST}"
}

function extract_coverage()
{
    local project_name="$1"
    local log_content="$2"

    echo "$log_content" | awk -v target="$project_name" '
    BEGIN { 
        max = 0 
        inside = 0 
    }
    
    /Entering directory/ {
        if ($0 ~ target) {
            inside = 1
        } else {
            inside = 0
        }
    }

    /Leaving directory/ {
        if ($0 ~ target) {
            inside = 0
        }
    }

    inside && /top-level declarations used/ {
        match($0, /[0-9]+%/)
        val = substr($0, RSTART, RLENGTH - 1) # Remove the % sign

        if (val + 0 > max + 0) {
            max = val
        }
    }

    END { 
        print max 
    }
    '
}

function check_coverage()
{
    local display_name="$1"
    local project_name="$2"
    local coverage_value="$3"

    _info "Checking $display_name Coverage..."

    if [[ -z "$coverage_value" || "$coverage_value" -eq 0 ]]; then
        _error "$display_name" "Failed to retrieve coverage for $project_name. (Check directory names in makefile output)"
        EXIT_CODE=$EXIT_FAILURE
        return
    fi

    if awk -v a="$coverage_value" -v b="$MINIMUM_COVERAGE" 'BEGIN{ exit !(a+0 >= b+0) }'; then
        _success "$display_name: $coverage_value% is >= $MINIMUM_COVERAGE% (PASS)"
    else
        _error "$display_name: $coverage_value% is < $MINIMUM_COVERAGE% (FAIL)" "Coverage below acceptable threshold."
        EXIT_CODE=$EXIT_FAILURE
    fi
}

LISP_COV=$(extract_coverage "LispLang" "$LOG_CONTENT")
check_coverage "Lisp (GLaDOS)" "LispLang" "$LISP_COV"

RUNE_COV=$(extract_coverage "RuneLang" "$LOG_CONTENT")
check_coverage "Rune" "RuneLang" "$RUNE_COV"

if [ $EXIT_CODE -eq $EXIT_SUCCESS ]; then
    echo -e "\n${GREEN}${BOLD}All projects meet the minimum coverage of $MINIMUM_COVERAGE%.${RST}"
else
    echo -e "\n${RED}${BOLD}One or more projects did not meet the minimum coverage of $MINIMUM_COVERAGE%.${RST}"
fi

exit $EXIT_CODE
