#!/usr/bin/env bash

# Kill on an error
set -e

COMPILER="../bin/japa"

echo
echo "#################### Running Benchmark for Japa compiler ####################"
echo

DURATIONCOMP=0
DURATIONCOMPOPT=0

DURATIONEXEC=0
DURATIONEXECOPT=0

# compile time without optimizations
for i in {0..9}
do
    rm -f fib-noOpt.cpp

    TIME="$({ time ../bin/japa -noOpt fib.japa ; } 2>&1 | grep "user" | sed -e "s/.*0m\([0-9]\).\([0-9]*\)s/\1\2/")"
    TIME=$[ $TIME ]
    DURATIONCOMP=$[ $DURATIONCOMP + $TIME ]
    ${COMPILER} -noOpt fib.japa > fib-noOpt.cpp
done

# compile time with optimizations
for i in {0..9}
do
    rm -f fib.cpp

    TIME="$({ time ../bin/japa fib.japa ; } 2>&1 | grep "user" | sed -e "s/.*0m\([0-9]\).\([0-9]*\)s/\1\2/")"
    TIME=$[ $TIME ]
    DURATIONCOMPOPT=$[ $DURATIONCOMPOPT + $TIME ]
    ${COMPILER} fib.japa > fib.cpp
done

g++ -O0 -o fib-noOpt fib-noOpt.cpp
g++ -O0 -o fib fib.cpp

# runningtime
for i in {0..9}
do
    TIME="$({ time ./fib-noOpt ; } 2>&1 | grep "user" | sed -e "s/.*0m\([0-9]\).\([0-9]*\)s/\1\2/")"
    TIME=$[ $TIME ]
    DURATIONEXEC=$[ $DURATIONEXEC + $TIME ]

    TIME="$({ time ./fib ; } 2>&1 | grep "user" | sed -e "s/.*0m\([0-9]\).\([0-9]*\)s/\1\2/")"
    TIME=$[ $TIME ]
    DURATIONEXECOPT=$[ $DURATIONEXECOPT + $TIME ]
done

COMP=$[ $DURATIONCOMP / 10 ]
COMP2=$[ $DURATIONCOMPOPT / 10 ]

EXEC=$[ $DURATIONEXEC / 10 ]
EXEC2=$[ $DURATIONEXECOPT / 10 ]

echo "Compilation (noOpt) $COMP ms"
echo "Execution   (noOpt) $EXEC ms"
echo "Compilation (opt)   $COMP2 ms"
echo "Execution   (opt)   $EXEC2 ms"