#!/usr/bin/env bash

# Kill on an error
set -e

COMPILER="../bin/japa"

OUTFOLD="tmp/"

PROGRAM1="../benchmarks/fib.japa"
PROGRAM2="../benchmarks/factorial.japa"
PROGRAM3="../benchmarks/perm-to-code.japa"

COPROGRAM1="fib.cpp"
CUPROGRAM1="fib-noOpt.cpp"
COPROGRAM2="factorial.cpp"
CUPROGRAM2="factorial-noOpt.cpp"
COPROGRAM3="perm-to-code.cpp"
CUPROGRAM3="perm-to-code-noOpt.cpp"

OEXEC1="fib"
UEXEC1="fib-noOpt"
OEXEC2="factorial"
UEXEC2="factorial-noOpt"
OEXEC3="perm-to-code"
UEXEC3="perm-to-code-noOpt"

OPROGRAM1DUR=0
OPROGRAM2DUR=0
OPROGRAM3DUR=0
UPROGRAM1DUR=0
UPROGRAM2DUR=0
UPROGRAM3DUR=0

echo
echo "#################### Preparing Runtime Benchmark for Japa Compiler ####################"
echo

# Ensuring the newest compiler build
make

mkdir -p $OUTFOLD
cd $OUTFOLD

# Compile to C++
$COMPILER -noOpt $PROGRAM1 > $CUPROGRAM1
$COMPILER -noOpt $PROGRAM2 > $CUPROGRAM2
$COMPILER -noOpt $PROGRAM3 > $CUPROGRAM3

$COMPILER $PROGRAM1 > $COPROGRAM1
$COMPILER $PROGRAM2 > $COPROGRAM2
$COMPILER $PROGRAM3 > $COPROGRAM3

# Compile executables
g++ -O0 -o $OEXEC1 $COPROGRAM1
g++ -O0 -o $UEXEC1 $CUPROGRAM1

g++ -O0 -o $OEXEC2 $COPROGRAM2
g++ -O0 -o $UEXEC2 $CUPROGRAM2

g++ -O0 -o $OEXEC3 $COPROGRAM3
g++ -O0 -o $UEXEC3 $CUPROGRAM3


echo
echo "#################### Running Runtime Benchmark for Japa Compiler ####################"
echo

N=10
for i in {1..$N}
do
    # Program 1 - optimized
    START=$(date +%N)
    ./$OEXEC1
    END=$(date +%N)
    OPROGRAM1DUR=$[ $OPROGRAM1DUR + $[ END - START ] ]

    # Program 1 - unoptimized
    START=$(date +%N)
    ./$UEXEC1
    END=$(date +%N)
    UPROGRAM1DUR=$[ $UPROGRAM1DUR + $[ END - START ] ]

    # Program 2 - optimized
    START=$(date +%N)
    ./$OEXEC2
    END=$(date +%N)
    OPROGRAM2DUR=$[ $OPROGRAM2DUR + $[ END - START ] ]

    # Program 2 - unoptimized
    START=$(date +%N)
    ./$UEXEC2
    END=$(date +%N)
    UPROGRAM2DUR=$[ $UPROGRAM2DUR + $[ END - START ] ]

    # Program 3 - optimized
    START=$(date +%N)
    ./$OEXEC3
    END=$(date +%N)
    OPROGRAM3DUR=$[ $OPROGRAM3DUR + $[ END - START ] ]

    # Program 3 - unoptimized
    START=$(date +%N)
    ./$UEXEC3
    END=$(date +%N)
    UPROGRAM3DUR=$[ $UPROGRAM3DUR + $[ END - START ] ]
done

echo
echo "#################### Cleaning up Runtime Benchmark for Japa Compiler ####################"
echo

cd ..
rm -r $OUTFOLD

echo
echo "#################### Runtime Benchmark for Japa Compiler Results ####################"
echo

ORES1=$[ $OPROGRAM1DUR / $N ]
ORES2=$[ $OPROGRAM2DUR / $N ]
ORES3=$[ $OPROGRAM3DUR / $N ]

URES1=$[ $UPROGRAM1DUR / $N ]
URES2=$[ $UPROGRAM2DUR / $N ]
URES3=$[ $UPROGRAM3DUR / $N ]

echo "Fibonacci:         $URES1"
echo "Fibonacci(opt):    $ORES1"
echo
echo "Factorial:         $URES2"
echo "Factorial(opt):    $ORES2"
echo
echo "Perm to code:      $URES3"
echo "Perm to code(opt): $ORES3"






# DURATIONCOMP=0
# DURATIONCOMPOPT=0

# DURATIONEXEC=0
# DURATIONEXECOPT=0

# # compile time without optimizations
# for i in {0..9}
# do
#     rm -f fib-noOpt.cpp

#     TIME="$({ time ../bin/japa -noOpt fib.japa ; } 2>&1 | grep "user" | sed -e "s/.*0m\([0-9]\).\([0-9]*\)s/\1\2/")"
#     TIME=$[ $TIME ]
#     DURATIONCOMP=$[ $DURATIONCOMP + $TIME ]
#     ${COMPILER} -noOpt fib.japa > fib-noOpt.cpp
# done

# # compile time with optimizations
# for i in {0..9}
# do
#     rm -f fib.cpp

#     TIME="$({ time ../bin/japa fib.japa ; } 2>&1 | grep "user" | sed -e "s/.*0m\([0-9]\).\([0-9]*\)s/\1\2/")"
#     TIME=$[ $TIME ]
#     DURATIONCOMPOPT=$[ $DURATIONCOMPOPT + $TIME ]
#     ${COMPILER} fib.japa > fib.cpp
# done

# g++ -O0 -o fib-noOpt fib-noOpt.cpp
# g++ -O0 -o fib fib.cpp

# # runningtime
# for i in {0..9}
# do
#     TIME="$({ time ./fib-noOpt ; } 2>&1 | grep "user" | sed -e "s/.*0m\([0-9]\).\([0-9]*\)s/\1\2/")"
#     TIME=$[ $TIME ]
#     DURATIONEXEC=$[ $DURATIONEXEC + $TIME ]

#     TIME="$({ time ./fib ; } 2>&1 | grep "user" | sed -e "s/.*0m\([0-9]\).\([0-9]*\)s/\1\2/")"
#     TIME=$[ $TIME ]
#     DURATIONEXECOPT=$[ $DURATIONEXECOPT + $TIME ]
# done

# COMP=$[ $DURATIONCOMP / 10 ]
# COMP2=$[ $DURATIONCOMPOPT / 10 ]

# EXEC=$[ $DURATIONEXEC / 10 ]
# EXEC2=$[ $DURATIONEXECOPT / 10 ]

# echo "Compilation (noOpt) $COMP ms"
# echo "Execution   (noOpt) $EXEC ms"
# echo "Compilation (opt)   $COMP2 ms"
# echo "Execution   (opt)   $EXEC2 ms"
