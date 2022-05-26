#!/usr/bin/env bash

# Run all the tests:
#   It works by comparing the result of the compiler on test programs,
#   with some preexamined gold files.

# Kill on an error
set -e

# Ensure newest compiler
make

base_dir="$(dirname "$0")/.."
compiler="$base_dir/bin/japa"
test_dir="$base_dir/tests"
gold_dir="$base_dir/goldFiles"

# $1 = program name
# $2 = gold file
compare () {
    if [ -f $2 ]; then
        local expected="$(cat $2)"
        if [[ "$output" != "$expected" ]]; then
          echo "Output for $0 does not match gold file."
          echo "Compare $1 with $2."
          return 1
        else
            return 0
        fi
    fi
    echo "gold file ($2) not present"
    return 1
}

file_len=0
for f in $test_dir/*; do
    L=$(basename "$f")
    if ((${#L} > $file_len)); then
        file_len=${#L}
    fi
done
file_len=$((file_len + 4))

echo
echo "#################### Running Tests for Japa compiler ####################"
echo

set +e

for f in $test_dir/*; do
    fname="$(basename "$f")"
    program_name="$(echo $fname | sed 's/.japa$//')"
    printf "%*s" $file_len " $fname:  "

    # Save both stderr and stdout in output, so negative tests can also be tested
    output=$($compiler $f 2>&1)
    if compare $fname "$gold_dir/$program_name.gold"; then
       echo -e "\033[92mSuccess.\033[0m"
    else
       echo -e "\033[91mTest error.\033[0m"
    fi
done
