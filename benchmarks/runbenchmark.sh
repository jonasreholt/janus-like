#!/usr/bin/env bash

# Kill on an error
set -e

COMPILER="../bin/japa"

OUTFOLD="tmp/"

PROGRAM1="../benchmarks/fib.japa"
PROGRAM2="../benchmarks/factorial.japa"
PROGRAM3="../benchmarks/perm-to-code.japa"
PROGRAM4="../benchmarks/encryption.japa"
PROGRAM5="../benchmarks/run-length-encoder.japa"

COPROGRAM1="fib.cpp"
CUPROGRAM1="fib-noOpt.cpp"
COPROGRAM2="factorial.cpp"
CUPROGRAM2="factorial-noOpt.cpp"
COPROGRAM3="perm-to-code.cpp"
CUPROGRAM3="perm-to-code-noOpt.cpp"
COPROGRAM4="encryption.cpp"
CUPROGRAM4="encryption-noOpt.cpp"
COPROGRAM5="run-length-encoder.cpp"
CUPROGRAM5="run-length-encoder-noOpt.cpp"

OEXEC1="fib"
UEXEC1="fib-noOpt"
OEXEC2="factorial"
UEXEC2="factorial-noOpt"
OEXEC3="perm-to-code"
UEXEC3="perm-to-code-noOpt"
OEXEC4="encryption"
UEXEC4="encryption-noOpt"
OEXEC5="run-length-encoder"
UEXEC5="run-length-encoder-noOpt"

OPROGRAM1DUR=0
OPROGRAM2DUR=0
OPROGRAM3DUR=0
OPROGRAM4DUR=0
OPROGRAM5DUR=0
UPROGRAM1DUR=0
UPROGRAM2DUR=0
UPROGRAM3DUR=0
UPROGRAM4DUR=0
UPROGRAM5DUR=0

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
$COMPILER -noOpt $PROGRAM4 > $CUPROGRAM4
$COMPILER -noOpt $PROGRAM5 > $CUPROGRAM5

$COMPILER $PROGRAM1 > $COPROGRAM1
$COMPILER $PROGRAM2 > $COPROGRAM2
$COMPILER $PROGRAM3 > $COPROGRAM3
$COMPILER $PROGRAM4 > $COPROGRAM4
$COMPILER $PROGRAM5 > $COPROGRAM5

# Compile executables
g++ -O0 -o $OEXEC1 $COPROGRAM1
g++ -O0 -o $UEXEC1 $CUPROGRAM1

g++ -O0 -o $OEXEC2 $COPROGRAM2
g++ -O0 -o $UEXEC2 $CUPROGRAM2

g++ -O0 -o $OEXEC3 $COPROGRAM3
g++ -O0 -o $UEXEC3 $CUPROGRAM3

g++ -O0 -o $OEXEC4 $COPROGRAM4
g++ -O0 -o $UEXEC4 $CUPROGRAM4

g++ -O0 -o $OEXEC5 $COPROGRAM5
g++ -O0 -o $UEXEC5 $CUPROGRAM5


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

    # Program 4 - optimized
    START=$(date +%N)
    ./$OEXEC4
    END=$(date +%N)
    OPROGRAM4DUR=$[ $OPROGRAM4DUR + $[ END - START ] ]

    # Program 4 - unoptimized
    START=$(date +%N)
    ./$UEXEC4
    END=$(date +%N)
    UPROGRAM4DUR=$[ $UPROGRAM4DUR + $[ END - START ] ]

    # Program 5 - optimized
    START=$(date +%N)
    ./$OEXEC5
    END=$(date +%N)
    OPROGRAM5DUR=$[ $OPROGRAM5DUR + $[ END - START ] ]

    # Program 5 - unoptimized
    START=$(date +%N)
    ./$UEXEC5
    END=$(date +%N)
    UPROGRAM5DUR=$[ $UPROGRAM5DUR + $[ END - START ] ]
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
ORES4=$[ $OPROGRAM4DUR / $N ]
ORES5=$[ $OPROGRAM5DUR / $N ]

URES1=$[ $UPROGRAM1DUR / $N ]
URES2=$[ $UPROGRAM2DUR / $N ]
URES3=$[ $UPROGRAM3DUR / $N ]
URES4=$[ $UPROGRAM4DUR / $N ]
URES5=$[ $UPROGRAM5DUR / $N ]

echo "Fibonacci:               $URES1"
echo "Fibonacci(opt):          $ORES1"
echo
echo "Factorial:               $URES2"
echo "Factorial(opt):          $ORES2"
echo
echo "Perm to code:            $URES3"
echo "Perm to code(opt):       $ORES3"
echo
echo "Encryption:              $URES4"
echo "Encryption(opt):         $ORES4"
echo
echo "Run length encoder:      $URES3"
echo "Run length encoder(opt): $ORES3"
