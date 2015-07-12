#!/usr/bin/env bash

if [ "x$1" = "x" ]
then
	echo "usage: $0 path/to/yamlkeysdiff"
fi

a=$(mktemp)
b=$(mktemp)

./randomyaml.py > "$a"
./randomyaml.py > "$b"

du -sh "$a" "$b"
env time "$1" "$a" "$b" > /dev/null

rm -f "$a" "$b"
