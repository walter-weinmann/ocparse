#!/bin/bash

read -p "Enter filer name of spec file [default value: all]: " $1

specs=$1

if [ "$#" -ne 1 ]; then
    specs=all
fi

# PATHS
logs="ct/logs"
paths="-pa"
paths=$paths" ebin"
paths=$paths" deps/*/ebin"

# Common Test start options
echo "------------------------------------------"
echo "Starting Common Test (Opts)"
echo "------------------------------------------"
echo "spec file(s) : $specs"
echo "EBIN Path    : $paths"
echo "log files    : $logs"
echo "------------------------------------------"

# Starting Common Test
ct_run -spec ct/${specs}.spec -logdir $logs -pa $paths
