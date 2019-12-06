.PHONY: examples

default: all

all: build examples

build:
	cabal new-build --write-ghc-environment-files always --enable-documentation

examples:
	ghc -fno-code examples/without_signals/*.hs
	ghc -fno-code examples/with_signals/*.hs

clean:
	cabal new-clean
