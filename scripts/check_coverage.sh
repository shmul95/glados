#!/bin/bash

GREEN="\033[1;32m"
RED="\033[1;31m"
ILC="\033[3m"
ORG="\033[1;33m"
RST="\033[0m"
BOLD="\033[1m"

MINIMUM_COVERAGE=80
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

    echo "$log_content" | awk -v proj="$project_name" '
        /Entering directory/ {
            if ($0 ~ proj) {
                in_project = 1
            } else {
                in_project = 0
            }
        }

        in_project && /Summary unified coverage report:/ {
            next_line_is_coverage = 1
            next
        }

        in_project && next_line_is_coverage && /% expressions used/ {
            print substr($1, 1, length($1)-1)
            exit
        }
    '
}

function check_coverage()
{
    local project_name="$1"
    local coverage_value="$2"

    _info "Checking $project_name Coverage"

    if [[ -z "$coverage_value" ]]; then
        _error "$project_name" "Failed to retrieve coverage. (Check log structure or script AWK pattern)"
    elif (( coverage_value >= MINIMUM_COVERAGE )); then
        _success "$project_name: $coverage_value% is greater than $MINIMUM_COVERAGE% (PASS)"
    else
        _error "$project_name: $coverage_value% is less than $MINIMUM_COVERAGE% (FAIL)" "Coverage below acceptable threshold."
        EXIT_CODE=$EXIT_FAILURE
    fi
}

LISP_COV=$(extract_coverage "Lisp" "$LOG_CONTENT")
check_coverage "Lisp (GLaDOS)" "$LISP_COV"

RUNE_COV=$(extract_coverage "Rune" "$LOG_CONTENT")
check_coverage "Rune" "$RUNE_COV"

if [ $EXIT_CODE -eq $EXIT_SUCCESS ]; then
    _success "All projects meet the minimum coverage of $MINIMUM_COVERAGE%."
else
    _error "Coverage check failed." "One or more projects did not meet the minimum coverage of $MINIMUM_COVERAGE%."
fi

exit $EXIT_CODE
