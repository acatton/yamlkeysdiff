
all: dist/build/yamlkeysdiff/yamlkeysdiff

cabal.sandbox.config:
	cabal sandbox init

dist/build/yamlkeysdiff/yamlkeysdiff: cabal.sandbox.config
	cabal install --only-dependencies
	cabal configure
	cabal build
