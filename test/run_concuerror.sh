#!/usr/bin/env bash

set -euo pipefail

cd -P -- "$(dirname -- "$0")/.."
CURDIR="$PWD"
BUILD_DIR="$CURDIR/_build"
CONCUERROR="${BUILD_DIR}/concuerror/bin/concuerror"
TESTS=( $(grep -oE '^[a-z0-9_]+_test()' test/concuerror_tests.erl) )

function banner() {
  if command -v figlet >/dev/null; then
    figlet "$@"
  else
    echo "$@"
  fi
}

function run_concuerror() {
  local test="$1"

  rm concuerror_report.txt || true

  APP_DIR="${BUILD_DIR}/concuerror+test/lib/ondemand"

  "$CONCUERROR" \
    --treat_as_normal spindown --treat_as_normal normal --treat_as_normal shutdown \
    --treat_as_normal boom \
    -x logger -x error_handler \
    --pa "${APP_DIR}/ebin" \
    --pa "${APP_DIR}/test/extra_src" \
    -f "${APP_DIR}/test/concuerror_tests.beam" \
    -t "$test" || {
    cat concuerror_report.txt
    exit 1
  }
}

while getopts ":t:" o; do
  case "${o}" in
    t)
      TESTS=("${OPTARG}")
      ;;
  esac
done
shift $((OPTIND-1))

for t in "${TESTS[@]}"; do
  banner running "$t"
  run_concuerror "$t"
done
