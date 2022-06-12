all:
	cabal build japa && cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/janus-like-0.1.0.0/x/japa/build/japa/japa bin/japa && cp bin/japa web

# This ignores the R^2 value
bench:
	cabal bench 2> /dev/null | grep "time\|mean\|std dev\|variance\|benchmarking"

# Benchmark the execution time of compiler
benchR:
	benchmarks/runbenchmark.sh

test:
	bin/tester.sh
