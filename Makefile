all : build

build :
	cabal v2-build

build-ghcjs :
	cabal v2-build -w /opt/ghcjs/8.4/bin/ghcjs --ghcjs

montecarlo-pi-time :
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.0.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.2.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.4.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.6.3 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.8.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-7.10.3 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-8.0.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-8.2.2 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-8.4.4 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)
	cabal v2-build montecarlo-pi --enable-tests -w ghc-8.6.5 > /dev/null
	time $$(cabal-plan list-bin montecarlo-pi)

generate-mix32 :
	cabal v2-build generate-mix32 && $$(cabal-plan list-bin generate-mix32)
