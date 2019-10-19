.PHONY: typecheck

typecheck:
	ghc -fno-code -isrc/ src/Control/*.hs
	ghc -fno-code -isrc/ src/Control/Runner/*.hs
	ghc -fno-code -isrc/ src/Control/SignalRunner/*.hs

	ghc -fno-code -isrc/ examples/with_signals/*.hs
	ghc -fno-code -isrc/ examples/without_signals/*.hs
