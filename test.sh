#!/bin/bash

set -e

make runtime

for f in test_inputs/bad/*.lat
do
    echo -n "$f... "

    set +e
    cargo run -- $f 2>/dev/null
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

for f in test_inputs/good/*.lat
do
    echo -n "$f... "

    set +e
    cargo run -- $f 2>/dev/null
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
    "$dir/$base.out" < "$dir/$base.input" > "$dir/$base.output2"
    diff "$dir/$base.output2" "$dir/$base.output"

    echo OK
done
