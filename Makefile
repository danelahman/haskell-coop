.PHONY: typecheck

typecheck:
	ghc -fno-code -isrc/ src/Control/Monad/*.hs
	ghc -fno-code -isrc/ src/Control/Monad/Comodel/*.hs

	ghc -fno-code -isrc/ examples/*.hs
