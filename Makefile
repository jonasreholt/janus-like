all:
	cabal build && cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/janus-like-0.1.0.0/x/janus-like/build/janus-like/janus-like bin/japa && cp bin/japa web
#	cd app && ghc Main && mv Main ../bin/japa && cp ../bin/japa ../web
