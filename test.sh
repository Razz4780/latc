#!/bin/bash

set -e

if (($# != 1)); then
	echo "Usage: $0 <tests root directory>"
	exit 1
fi

make

for f in "$1"/good/*.lat
do
    echo -n "$f... "

    set +e
    ./latc $f 1>/dev/null
    res=$?
    set -e

    if [ $res == 1 ]
    then
        echo FAILED
        exit 1
    fi

    if [ $res != 0 ]
    then
        echo PANICKED
        exit 1
    fi

    base=$(basename $f .lat)
    dir=$(dirname $f)
    "$dir/$base" < "$dir/$base.input" > "$dir/$base.out"
    diff "$dir/$base.out" "$dir/$base.output"

    echo OK
done

for f in "$1"/bad/*.lat
do
    echo -n "$f... "

    set +e
    ./latc $f
    res=$?
    set -e

    if [ $res == 0 ]
    then
        echo FAILED
        exit 1
    fi

    if [ $res != 1 ]
    then
        echo PANICKED
        exit 1
    fi

    echo OK
done
