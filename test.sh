#!/usr/bin/env bash

## PLEASE REPLACE ME WITH A PROPER TEST SUITE :)

if [[ -z "$CHARTA" ]]; then
    CHARTA=$(cabal list-bin charta)
fi

test_for () {
    local result=$($CHARTA $1)
    local expect=""
    printf -v expect "$2"
    if [[ "$result" = "$expect" ]]; then
        echo "[✓] Passed $1"
    else
        echo "[×] Failed $1"
        diff -u <(printf '%s\n' "$expect") <(printf '%s\n' "$result")
    fi
}

test_for "examples/aoc-1.ch" "Answer:\n969.0"
test_for "examples/fibo.ch" "[0.0, 1.0, 1.0, 2.0, 3.0, 5.0, 8.0, 13.0, 21.0, 34.0, 55.0]"
test_for "examples/piglatin.ch" "anmay siay ondemnedcay otay ebay reefay"
test_for "examples/core-fns.ch" "stack-ops:\nOK\nstack-ops-2:\nOK\narithmetics-comparisons:\nOK\nconversions:\nOK\ntypes:\nOK\nmixed-fns:\nOK"
