#!/bin/bash

read -p "Mandatory: Enter filer name of test suite [ct/xxx_SUITE]: " suite
read -p "Optional : Enter filer name of test case group(s) [list]: " group
read -p "Optional : Enter filer name of test case(s)       [list]: " case

# PATHS
logs="ct/logs"
paths="-pa"
paths=$paths" ebin"
paths=$paths" deps/*/ebin"

# Common Test start options
echo "------------------------------------------"
echo "Starting Common Test (Opts)"
echo "------------------------------------------"
echo "test suite         : $suite"
echo "test case group(s) : $group"
echo "test case(s)       : $case"
echo "spec file(s)       : $specs"
echo "EBIN Path          : $paths"
echo "log files          : $logs"
echo "------------------------------------------"

# Starting Common Test
if [ "$suite" = "" ]; then
    echo ""
    echo "========================================"
    echo "== Error: No test suite specified !!! =="
    echo "========================================"
    exit 1
fi

if [ "$group" = "" ]; then
    if [ "$case" = "" ]; then
        ct_run -suite ct/${suite}_SUITE -logdir $logs -pa $paths
    else
        ct_run -suite ct/${suite}_SUITE -case $case -logdir $logs -pa $paths
    fi
    exit 0
fi

if [ "$case" = "" ]; then
    ct_run -suite ct/${suite}_SUITE -group [$group] -logdir $logs -pa $paths
else
    ct_run -suite ct/${suite}_SUITE -group [$group] -case $case -logdir $logs -pa $paths
fi
