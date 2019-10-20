.PHONY: build

build:
	cabal new-build --enable-documentation

typecheck:
	ghc -fno-code src/Control/*.hs
	ghc -fno-code src/Control/Runner/*.hs
	ghc -fno-code src/Control/SignalRunner/*.hs

	ghc -fno-code examples/with_signals/*.hs
	ghc -fno-code examples/without_signals/*.hs
