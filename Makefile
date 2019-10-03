.PHONY: typecheck

typecheck:
	ghc -fno-code -isrc/ src/Control/Monad/*.hs
	ghc -fno-code -isrc/ src/Control/Monad/Runner/*.hs

	ghc -fno-code -isrc/ examples/*.hs
